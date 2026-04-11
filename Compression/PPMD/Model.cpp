/****************************************************************************
 *  This file is part of PPMd project                                       *
 *  Written and distributed to public domain by Dmitry Shkarin 1997,        *
 *  1999-2001                                                               *
 *  Contents: PPMII model description and encoding/decoding routines        *
 ****************************************************************************/
#include <string.h>
#include "PPMd.h"
#pragma hdrstop
#include "Coder.hpp"
#include "SubAlloc.hpp"

enum { UP_FREQ=5, INT_BITS=7, PERIOD_BITS=7, TOT_BITS=INT_BITS+PERIOD_BITS,
    INTERVAL=1 << INT_BITS, BIN_SCALE=1 << TOT_BITS, MAX_FREQ=124, O_BOUND=9 };

#pragma pack(1)
static struct SEE2_CONTEXT { // SEE-contexts for PPM-contexts with masked symbols
    WORD Summ;
    BYTE Shift, Count;
    void init(UINT InitVal) { Summ=InitVal << (Shift=PERIOD_BITS-4); Count=7; }
    UINT getMean() {
        UINT RetVal=(Summ >> Shift);        Summ -= RetVal;
        return RetVal+(RetVal == 0);
    }
    void update() {
        if (Shift < PERIOD_BITS && --Count == 0) {
            Summ += Summ;                   Count=3 << Shift++;
        }
    }
} _PACK_ATTR SEE2Cont[24][32], DummySEE2Cont;
/* PPM_CONTEXT uses 32-bit heap offsets (CTX_REF/STATE_REF) instead of raw
   pointers so the struct stays at UNIT_SIZE=12 bytes on both 32 and 64-bit.
   Use PPCTX/RPCTX/PPSTAT/RPSTAT helpers defined below to convert.         */
static struct PPM_CONTEXT {                 // Notes:
    BYTE NumStats, Flags;                   // 1. NumStats & NumMasked contain
    WORD SummFreq;                          //  number of symbols minus 1
    struct STATE {                          // 2. sizeof(WORD) > sizeof(BYTE)
        BYTE Symbol, Freq;                  // 3. contexts example:
        CTX_REF Successor;                  // MaxOrder:  (was PPM_CONTEXT*)
    } _PACK_ATTR;                           //  ABCD    context
    STATE_REF Stats;                        //   BCD    suffix (was STATE*)
    CTX_REF   Suffix;                       //   BCDE   successor (was PPM_CONTEXT*)
    inline void encodeBinSymbol(int symbol);// other orders:
    inline void   encodeSymbol1(int symbol);//   BCD    context
    inline void   encodeSymbol2(int symbol);//    CD    suffix
    inline void           decodeBinSymbol();//   BCDE   successor
    inline void             decodeSymbol1();
    inline void             decodeSymbol2();
    inline void           update1(STATE* p);
    inline void           update2(STATE* p);
    inline SEE2_CONTEXT*     makeEscFreq2();
    void                          rescale();
    void      refresh(int OldNU,BOOL Scale);
    PPM_CONTEXT*          cutOff(int Order);
    PPM_CONTEXT*  removeBinConts(int Order);
    STATE& oneState() const { return (STATE&) SummFreq; }
} _PACK_ATTR* MaxContext;
#pragma pack()

/* Ref <-> pointer converters for the 32-bit intra-heap offset scheme.
   Refs are 1-based (r = byte_offset_from_HeapStart + 1).
   CTX_REF=0 / STATE_REF=0 are the NULL sentinels.
   This means HeapStart itself maps to ref=1 (not 0), allowing PPMD to store
   pText=HeapStart in Successor without confusing it with NULL.             */
inline PPM_CONTEXT*
    PPCTX(CTX_REF r) { return r?(PPM_CONTEXT*)(HeapStart+r-1):(PPM_CONTEXT*)0; }
inline CTX_REF
    RPCTX(const PPM_CONTEXT* p) { return p?(CTX_REF)((BYTE*)p-HeapStart+1):0; }
inline CTX_REF
    BREF(const void* p) { return p?(CTX_REF)((BYTE*)p-HeapStart+1):0; }
inline PPM_CONTEXT::STATE*
    PPSTAT(STATE_REF r) { return r?(PPM_CONTEXT::STATE*)(HeapStart+r-1):(PPM_CONTEXT::STATE*)0; }
inline STATE_REF
    RPSTAT(const PPM_CONTEXT::STATE* p) { return p?(STATE_REF)((BYTE*)p-HeapStart+1):0; }
/* True when ref falls inside the unit-storage region (>= UnitsStart). */
inline bool GE_UNITS(CTX_REF r) { return r && HeapStart+r-1 >= UnitsStart; }

static BYTE NS2BSIndx[256], QTable[260];    // constants
static PPM_CONTEXT::STATE* FoundState;      // found next state transition
static int  InitEsc, OrderFall, RunLength, InitRL, MaxOrder;
static BYTE CharMask[256], NumMasked, PrevSuccess, EscCount, PrintCount;
static WORD BinSumm[25][64];                // binary SEE-contexts
static MR_METHOD MRMethod;

inline void SWAP(PPM_CONTEXT::STATE& s1,PPM_CONTEXT::STATE& s2)
{
    WORD t1=(WORD&) s1;                     CTX_REF t2=s1.Successor;
    (WORD&) s1 = (WORD&) s2;                s1.Successor=s2.Successor;
    (WORD&) s2 = t1;                        s2.Successor=t2;
}
inline void StateCpy(PPM_CONTEXT::STATE& s1,const PPM_CONTEXT::STATE& s2)
{
    (WORD&) s1=(WORD&) s2;                  s1.Successor=s2.Successor;
}
struct PPMD_STARTUP { inline PPMD_STARTUP(); } PPMd_StartUp;
inline PPMD_STARTUP::PPMD_STARTUP()         // constants initialization
{
    UINT i, k, m, Step;
    for (i=0,k=1;i < N1     ;i++,k += 1)    Indx2Units[i]=k;
    for (k++;i < N1+N2      ;i++,k += 2)    Indx2Units[i]=k;
    for (k++;i < N1+N2+N3   ;i++,k += 3)    Indx2Units[i]=k;
    for (k++;i < N1+N2+N3+N4;i++,k += 4)    Indx2Units[i]=k;
    for (k=i=0;k < 128;k++) {
        i += (Indx2Units[i] < k+1);         Units2Indx[k]=i;
    }
    NS2BSIndx[0]=2*0;                       NS2BSIndx[1]=2*1;
    memset(NS2BSIndx+2,2*2,9);              memset(NS2BSIndx+11,2*3,256-11);
    for (i=0;i < UP_FREQ;i++)               QTable[i]=i;
    for (m=i=UP_FREQ, k=Step=1;i < 260;i++) {
        QTable[i]=m;
        if ( !--k ) { k = ++Step;           m++; }
    }
    (unsigned int&) DummySEE2Cont=(unsigned int)PPMdSignature;
}
static void _STDCALL StartModelRare(int _MaxOrder,MR_METHOD _MRMethod)
{
    UINT i, k, m;
    memset(CharMask,0,sizeof(CharMask));    EscCount=PrintCount=1;
    if (_MaxOrder < 2) {                    // we are in solid mode
        OrderFall=MaxOrder;
        for (PPM_CONTEXT* pc=MaxContext;pc->Suffix != 0;pc=PPCTX(pc->Suffix))
                OrderFall--;
        return;
    }
    OrderFall=MaxOrder=_MaxOrder;           MRMethod=_MRMethod;
    InitSubAllocator();
    RunLength=InitRL=-((MaxOrder < 12)?MaxOrder:12)-1;
    MaxContext = (PPM_CONTEXT*) AllocContext();
    MaxContext->Suffix=0;
    MaxContext->SummFreq=(MaxContext->NumStats=255)+2;
    MaxContext->Stats = RPSTAT((PPM_CONTEXT::STATE*) AllocUnits(256/2));
    { PPM_CONTEXT::STATE* p=PPSTAT(MaxContext->Stats);
    for (PrevSuccess=i=0;i < 256;i++) {
        p[i].Symbol=i;                      p[i].Freq=1;
        p[i].Successor=0;
    }}
static const WORD InitBinEsc[]={0x3CDD,0x1F3F,0x59BF,0x48F3,0x64A1,0x5ABC,0x6632,0x6051};
    for (i=m=0;m < 25;m++) {
        while (QTable[i] == m)              i++;
        for (k=0;k < 8;k++)
                BinSumm[m][k]=BIN_SCALE-InitBinEsc[k]/(i+1);
        for (k=8;k < 64;k += 8)
                memcpy(BinSumm[m]+k,BinSumm[m],8*sizeof(WORD));
    }
    for (i=m=0;m < 24;m++) {
        while (QTable[i+3] == m+3)          i++;
        SEE2Cont[m][0].init(2*i+5);
        for (k=1;k < 32;k++)                SEE2Cont[m][k]=SEE2Cont[m][0];
    }
}
void PPM_CONTEXT::refresh(int OldNU,BOOL Scale)
{
    int i=NumStats, EscFreq;
    STATE* p; Stats=RPSTAT(p=(STATE*)ShrinkUnits(PPSTAT(Stats),OldNU,(i+2)>>1));
    Flags=(Flags & (0x10+0x04*Scale))+0x08*(p->Symbol >= 0x40);
    EscFreq=SummFreq-p->Freq;
    SummFreq = (p->Freq=(p->Freq+Scale) >> Scale);
    do {
        EscFreq -= (++p)->Freq;
        SummFreq += (p->Freq=(p->Freq+Scale) >> Scale);
        Flags |= 0x08*(p->Symbol >= 0x40);
    } while ( --i );
    SummFreq += (EscFreq=(EscFreq+Scale) >> Scale);
}
#define P_CALL(F) do { PPM_CONTEXT* _ps=PPCTX(p->Successor); \
    PrefetchData(_ps); p->Successor=RPCTX(_ps->F(Order+1)); } while(0)
PPM_CONTEXT* PPM_CONTEXT::cutOff(int Order)
{
    int i, tmp;
    STATE* p;
    if ( !NumStats ) {
        if (GE_UNITS((p=&oneState())->Successor)) {
            if (Order < MaxOrder)           P_CALL(cutOff);
            else                            p->Successor=0;
            if (!p->Successor && Order > O_BOUND)
                    goto REMOVE;
            return this;
        } else {
REMOVE:     SpecialFreeUnit(this);          return NULL;
        }
    }
    { STATE* sBase;
    PrefetchData(PPSTAT(Stats));
    Stats=RPSTAT(sBase=(STATE*)MoveUnitsUp(PPSTAT(Stats),tmp=(NumStats+2)>>1));
    for (p=sBase+(i=NumStats);p >= sBase;p--)
            if (!GE_UNITS(p->Successor)) {
                p->Successor=0;             SWAP(*p,sBase[i--]);
            } else if (Order < MaxOrder)    P_CALL(cutOff);
            else                            p->Successor=0;
    if (i != NumStats && Order) {
        NumStats=i;                         p=sBase;
        if (i < 0) { FreeUnits(p,tmp);      goto REMOVE; }
        else if (i == 0) {
            Flags=(Flags & 0x10)+0x08*(p->Symbol >= 0x40);
            StateCpy(oneState(),*p);        FreeUnits(p,tmp);
            oneState().Freq=(oneState().Freq+11) >> 3;
        } else                              refresh(tmp,SummFreq > 16*i);
    }}
    return this;
}
PPM_CONTEXT* PPM_CONTEXT::removeBinConts(int Order)
{
    STATE* p;
    if ( !NumStats ) {
        p=&oneState();
        if (GE_UNITS(p->Successor) && Order < MaxOrder)
                P_CALL(removeBinConts);
        else                                p->Successor=0;
        if (!p->Successor && (!PPCTX(Suffix)->NumStats || PPCTX(Suffix)->Flags == 0xFF)) {
            FreeUnits(this,1);              return NULL;
        } else                              return this;
    }
    { STATE* sBase=PPSTAT(Stats);
    PrefetchData(sBase);
    for (p=sBase+NumStats;p >= sBase;p--)
            if (GE_UNITS(p->Successor) && Order < MaxOrder)
                    P_CALL(removeBinConts);
            else                            p->Successor=0;}
    return this;
}
static void RestoreModelRare(PPM_CONTEXT* pc1,PPM_CONTEXT* MinContext,
        PPM_CONTEXT* FSuccessor)
{
    PPM_CONTEXT* pc;
    PPM_CONTEXT::STATE* p;
    for (pc=MaxContext, pText=HeapStart;pc != pc1;pc=PPCTX(pc->Suffix))
            if (--(pc->NumStats) == 0) {
                pc->Flags=(pc->Flags & 0x10)+0x08*(PPSTAT(pc->Stats)->Symbol >= 0x40);
                p=PPSTAT(pc->Stats);        StateCpy(pc->oneState(),*p);
                SpecialFreeUnit(p);
                pc->oneState().Freq=(pc->oneState().Freq+11) >> 3;
            } else
                    pc->refresh((pc->NumStats+3) >> 1,FALSE);
    for ( ;pc != MinContext;pc=PPCTX(pc->Suffix))
            if ( !pc->NumStats )
                    pc->oneState().Freq -= pc->oneState().Freq >> 1;
            else if ((pc->SummFreq += 4) > 128+4*pc->NumStats)
                    pc->refresh((pc->NumStats+2) >> 1,TRUE);
    if (MRMethod > MRM_FREEZE) {
        MaxContext=FSuccessor;              GlueCount += !(BList[1].Stamp & 1);
    } else if (MRMethod == MRM_FREEZE) {
        while ( MaxContext->Suffix )        MaxContext=PPCTX(MaxContext->Suffix);
        MaxContext->removeBinConts(0);      MRMethod=MR_METHOD(MRMethod+1);
        GlueCount=0;                        OrderFall=MaxOrder;
    } else if (MRMethod == MRM_RESTART || GetUsedMemory() < (SubAllocatorSize >> 1)) {
        StartModelRare(MaxOrder,MRMethod);
        EscCount=0;                         PrintCount=0xFF;
    } else {
        while ( MaxContext->Suffix )        MaxContext=PPCTX(MaxContext->Suffix);
        do {
            MaxContext->cutOff(0);          ExpandTextArea();
        } while (GetUsedMemory() > 3*(SubAllocatorSize >> 2));
        GlueCount=0;                        OrderFall=MaxOrder;
    }
}
static PPM_CONTEXT* _FASTCALL CreateSuccessors(BOOL Skip,PPM_CONTEXT::STATE* p,
        PPM_CONTEXT* pc);
static PPM_CONTEXT* _FASTCALL ReduceOrder(PPM_CONTEXT::STATE* p,PPM_CONTEXT* pc)
{
    PPM_CONTEXT::STATE* p1,  * ps[MAX_O], ** pps=ps;
    PPM_CONTEXT* pc1=pc;
    CTX_REF UpBranch = BREF(pText);
    BYTE tmp, sym=FoundState->Symbol;
    *pps++ = FoundState;                    FoundState->Successor=UpBranch;
    OrderFall++;
    if ( p ) { pc=PPCTX(pc->Suffix);        goto LOOP_ENTRY; }
    for ( ; ; ) {
        if ( !pc->Suffix ) {
            if (MRMethod > MRM_FREEZE) {
FROZEN:         do { (*--pps)->Successor = RPCTX(pc); } while (pps != ps);
                pText=HeapStart+1;          OrderFall=1;
            }
            return pc;
        }
        pc=PPCTX(pc->Suffix);
        if ( pc->NumStats ) {
            if ((p=PPSTAT(pc->Stats))->Symbol != sym)
                    do { tmp=p[1].Symbol;   p++; } while (tmp != sym);
            tmp=2*(p->Freq < MAX_FREQ-9);
            p->Freq += tmp;                 pc->SummFreq += tmp;
        } else { p=&(pc->oneState());       p->Freq += (p->Freq < 32); }
LOOP_ENTRY:
        if ( p->Successor )                 break;
        *pps++ = p;                         p->Successor=UpBranch;
        OrderFall++;
    }
    if (MRMethod > MRM_FREEZE) {
        pc = PPCTX(p->Successor);           goto FROZEN;
    } else if (p->Successor <= UpBranch) {
        p1=FoundState;                      FoundState=p;
        p->Successor=RPCTX(CreateSuccessors(FALSE,NULL,pc));
        FoundState=p1;
    }
    if (OrderFall == 1 && pc1 == MaxContext) {
        FoundState->Successor=p->Successor; pText--;
    }
    return PPCTX(p->Successor);
}
void PPM_CONTEXT::rescale()
{
    UINT OldNU, Adder, EscFreq, i=NumStats;
    STATE tmp, * p1, * p;
    STATE* sBase=PPSTAT(Stats);
    for (p=FoundState;p != sBase;p--)       SWAP(p[0],p[-1]);
    p->Freq += 4;                           SummFreq += 4;
    EscFreq=SummFreq-p->Freq;
    Adder=(OrderFall != 0 || MRMethod > MRM_FREEZE);
    SummFreq = (p->Freq=(p->Freq+Adder) >> 1);
    do {
        EscFreq -= (++p)->Freq;
        SummFreq += (p->Freq=(p->Freq+Adder) >> 1);
        if (p[0].Freq > p[-1].Freq) {
            StateCpy(tmp,*(p1=p));
            do StateCpy(p1[0],p1[-1]); while (tmp.Freq > (--p1)[-1].Freq);
            StateCpy(*p1,tmp);
        }
    } while ( --i );
    if (p->Freq == 0) {
        do { i++; } while ((--p)->Freq == 0);
        EscFreq += i;                       OldNU=(NumStats+2) >> 1;
        if ((NumStats -= i) == 0) {
            StateCpy(tmp,*sBase);
            tmp.Freq=(2*tmp.Freq+EscFreq-1)/EscFreq;
            if (tmp.Freq > MAX_FREQ/3)      tmp.Freq=MAX_FREQ/3;
            FreeUnits(sBase,OldNU);         StateCpy(oneState(),tmp);
            Flags=(Flags & 0x10)+0x08*(tmp.Symbol >= 0x40);
            FoundState=&oneState();         return;
        }
        Stats=RPSTAT(sBase=(STATE*)ShrinkUnits(sBase,OldNU,(NumStats+2)>>1));
        Flags &= ~0x08;                     i=NumStats;
        Flags |= 0x08*((p=sBase)->Symbol >= 0x40);
        do { Flags |= 0x08*((++p)->Symbol >= 0x40); } while ( --i );
    }
    SummFreq += (EscFreq -= (EscFreq >> 1));
    Flags |= 0x04;                          FoundState=sBase;
}
static PPM_CONTEXT* _FASTCALL CreateSuccessors(BOOL Skip,PPM_CONTEXT::STATE* p,
        PPM_CONTEXT* pc)
{
    PPM_CONTEXT ct;
    CTX_REF UpBranch=FoundState->Successor;
    PPM_CONTEXT::STATE* ps[MAX_O], ** pps=ps;
    UINT cf, s0;
    BYTE tmp, sym=FoundState->Symbol;
    if ( !Skip ) {
        *pps++ = FoundState;
        if ( !pc->Suffix )                  goto NO_LOOP;
    }
    if ( p ) { pc=PPCTX(pc->Suffix);        goto LOOP_ENTRY; }
    do {
        pc=PPCTX(pc->Suffix);
        if ( pc->NumStats ) {
            if ((p=PPSTAT(pc->Stats))->Symbol != sym)
                    do { tmp=p[1].Symbol;   p++; } while (tmp != sym);
            tmp=(p->Freq < MAX_FREQ-9);
            p->Freq += tmp;                 pc->SummFreq += tmp;
        } else {
            p=&(pc->oneState());
            p->Freq += (!PPCTX(pc->Suffix)->NumStats & (p->Freq < 24));
        }
LOOP_ENTRY:
        if (p->Successor != UpBranch) {
            pc=PPCTX(p->Successor);         break;
        }
        *pps++ = p;
    } while ( pc->Suffix );
NO_LOOP:
    if (pps == ps)                          return pc;
    ct.NumStats=0;                          ct.Flags=0x10*(sym >= 0x40);
    ct.oneState().Symbol=sym=*(HeapStart+UpBranch-1);
    ct.oneState().Successor=UpBranch+1;
    ct.Flags |= 0x08*(sym >= 0x40);
    if ( pc->NumStats ) {
        if ((p=PPSTAT(pc->Stats))->Symbol != sym)
                do { tmp=p[1].Symbol;       p++; } while (tmp != sym);
        s0=pc->SummFreq-pc->NumStats-(cf=p->Freq-1);
        ct.oneState().Freq=1+((2*cf <= s0)?(5*cf > s0):((cf+2*s0-3)/s0));
    } else
            ct.oneState().Freq=pc->oneState().Freq;
    do {
        PPM_CONTEXT* pc1 = (PPM_CONTEXT*) AllocContext();
        if ( !pc1 )                         return NULL;
        ((unsigned int*) pc1)[0] = ((unsigned int*) &ct)[0];
        ((unsigned int*) pc1)[1] = ((unsigned int*) &ct)[1];
        ((unsigned int*) pc1)[2] = ((unsigned int*) &ct)[2];
        pc1->Suffix=RPCTX(pc);
        (*--pps)->Successor=RPCTX(pc=pc1);
    } while (pps != ps);
    return pc;
}
static inline void UpdateModel(PPM_CONTEXT* MinContext)
{
    PPM_CONTEXT::STATE* p=NULL;
    PPM_CONTEXT* Successor, * FSuccessor, * pc, * pc1=MaxContext;
    UINT ns1, ns, cf, sf, s0, FFreq=FoundState->Freq;
    BYTE Flag, sym, FSymbol=FoundState->Symbol;
    FSuccessor=PPCTX(FoundState->Successor); pc=PPCTX(MinContext->Suffix);
    if (FFreq < MAX_FREQ/4 && pc) {
        if ( pc->NumStats ) {
            if ((p=PPSTAT(pc->Stats))->Symbol != FSymbol) {
                do { sym=p[1].Symbol;       p++; } while (sym != FSymbol);
                if (p[0].Freq >= p[-1].Freq) {
                    SWAP(p[0],p[-1]);       p--;
                }
            }
            cf=2*(p->Freq < MAX_FREQ-9);
            p->Freq += cf;                  pc->SummFreq += cf;
        } else { p=&(pc->oneState());       p->Freq += (p->Freq < 32); }
    }
    if (!OrderFall && FSuccessor) {
        FoundState->Successor=RPCTX(CreateSuccessors(TRUE,p,MinContext));
        if ( !FoundState->Successor )       goto RESTART_MODEL;
        MaxContext=PPCTX(FoundState->Successor); return;
    }
    *pText++ = FSymbol;                     Successor = (PPM_CONTEXT*) pText;
    if (pText >= UnitsStart)                goto RESTART_MODEL;
    if ( FSuccessor ) {
        if ((BYTE*) FSuccessor < UnitsStart)
                FSuccessor=CreateSuccessors(FALSE,p,MinContext);
    } else
                FSuccessor=ReduceOrder(p,MinContext);
    if ( !FSuccessor )                      goto RESTART_MODEL;
    if ( !--OrderFall ) {
        Successor=FSuccessor;               pText -= (MaxContext != MinContext);
    } else if (MRMethod > MRM_FREEZE) {
        Successor=FSuccessor;               pText=HeapStart;
        OrderFall=0;
    }
    s0=MinContext->SummFreq-(ns=MinContext->NumStats)-FFreq;
    for (Flag=0x08*(FSymbol >= 0x40);pc1 != MinContext;pc1=PPCTX(pc1->Suffix)) {
        if ((ns1=pc1->NumStats) != 0) {
            if ((ns1 & 1) != 0) {
                p=(PPM_CONTEXT::STATE*) ExpandUnits(PPSTAT(pc1->Stats),(ns1+1) >> 1);
                if ( !p )                   goto RESTART_MODEL;
                pc1->Stats=RPSTAT(p);
            }
            pc1->SummFreq += (3*ns1+1 < ns);
        } else {
            p=(PPM_CONTEXT::STATE*) AllocUnits(1);
            if ( !p )                       goto RESTART_MODEL;
            StateCpy(*p,pc1->oneState());   pc1->Stats=RPSTAT(p);
            if (p->Freq < MAX_FREQ/4-1)     p->Freq += p->Freq;
            else                            p->Freq  = MAX_FREQ-4;
            pc1->SummFreq=p->Freq+InitEsc+(ns > 2);
        }
        cf=2*FFreq*(pc1->SummFreq+6);       sf=s0+pc1->SummFreq;
        if (cf < 6*sf) {
            cf=1+(cf > sf)+(cf >= 4*sf);
            pc1->SummFreq += 4;
        } else {
            cf=4+(cf > 9*sf)+(cf > 12*sf)+(cf > 15*sf);
            pc1->SummFreq += cf;
        }
        p=PPSTAT(pc1->Stats)+(++pc1->NumStats); p->Successor=RPCTX(Successor);
        p->Symbol = FSymbol;                p->Freq = cf;
        pc1->Flags |= Flag;
    }
    MaxContext=FSuccessor;                  return;
RESTART_MODEL:
    RestoreModelRare(pc1,MinContext,FSuccessor);
}
// Tabulated escapes for exponential symbol distribution
static const BYTE ExpEscape[16]={ 25,14, 9, 7, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 2 };
#define GET_MEAN(SUMM,SHIFT,ROUND) ((SUMM+(1 << (SHIFT-ROUND))) >> (SHIFT))
inline void PPM_CONTEXT::encodeBinSymbol(int symbol)
{
    BYTE indx=NS2BSIndx[PPCTX(Suffix)->NumStats]+PrevSuccess+Flags;
    STATE& rs=oneState();
    WORD& bs=BinSumm[QTable[rs.Freq-1]][indx+((RunLength >> 26) & 0x20)];
    if (rs.Symbol == symbol) {
        FoundState=&rs;                     rs.Freq += (rs.Freq < 196);
        SubRange.LowCount=0;                SubRange.HighCount=bs;
        bs += INTERVAL-GET_MEAN(bs,PERIOD_BITS,2);
        PrevSuccess=1;                      RunLength++;
    } else {
        SubRange.LowCount=bs;               bs -= GET_MEAN(bs,PERIOD_BITS,2);
        SubRange.HighCount=BIN_SCALE;       InitEsc=ExpEscape[bs >> 10];
        CharMask[rs.Symbol]=EscCount;
        NumMasked=PrevSuccess=0;            FoundState=NULL;
    }
}
inline void PPM_CONTEXT::decodeBinSymbol()
{
    BYTE indx=NS2BSIndx[PPCTX(Suffix)->NumStats]+PrevSuccess+Flags;
    STATE& rs=oneState();
    WORD& bs=BinSumm[QTable[rs.Freq-1]][indx+((RunLength >> 26) & 0x20)];
    if (ariGetCurrentShiftCount(TOT_BITS) < bs) {
        FoundState=&rs;                     rs.Freq += (rs.Freq < 196);
        SubRange.LowCount=0;                SubRange.HighCount=bs;
        bs += INTERVAL-GET_MEAN(bs,PERIOD_BITS,2);
        PrevSuccess=1;                      RunLength++;
    } else {
        SubRange.LowCount=bs;               bs -= GET_MEAN(bs,PERIOD_BITS,2);
        SubRange.HighCount=BIN_SCALE;       InitEsc=ExpEscape[bs >> 10];
        CharMask[rs.Symbol]=EscCount;
        NumMasked=PrevSuccess=0;            FoundState=NULL;
    }
}
inline void PPM_CONTEXT::update1(STATE* p)
{
    (FoundState=p)->Freq += 4;              SummFreq += 4;
    if (p[0].Freq > p[-1].Freq) {
        SWAP(p[0],p[-1]);                   FoundState=--p;
        if (p->Freq > MAX_FREQ)             rescale();
    }
}
inline void PPM_CONTEXT::encodeSymbol1(int symbol)
{
    STATE* p=PPSTAT(Stats);
    UINT LoCnt, i=p->Symbol;               SubRange.scale=SummFreq;
    if (i == symbol) {
        PrevSuccess=(2*(SubRange.HighCount=p->Freq) >= SubRange.scale);
        (FoundState=p)->Freq += 4;          SummFreq += 4;
        RunLength += PrevSuccess;
        if (p->Freq > MAX_FREQ)             rescale();
        SubRange.LowCount=0;                return;
    }
    LoCnt=p->Freq;
    i=NumStats;                             PrevSuccess=0;
    while ((++p)->Symbol != symbol) {
        LoCnt += p->Freq;
        if (--i == 0) {
            if ( Suffix )                   PrefetchData(PPCTX(Suffix));
            SubRange.LowCount=LoCnt;        CharMask[p->Symbol]=EscCount;
            i=NumMasked=NumStats;           FoundState=NULL;
            do { CharMask[(--p)->Symbol]=EscCount; } while ( --i );
            SubRange.HighCount=SubRange.scale;
            return;
        }
    }
    SubRange.HighCount=(SubRange.LowCount=LoCnt)+p->Freq;
    update1(p);
}
inline void PPM_CONTEXT::decodeSymbol1()
{
    STATE* p=PPSTAT(Stats);
    UINT i, count, HiCnt=p->Freq;          SubRange.scale=SummFreq;
    if ((count=ariGetCurrentCount()) < HiCnt) {
        PrevSuccess=(2*(SubRange.HighCount=HiCnt) >= SubRange.scale);
        (FoundState=p)->Freq=(HiCnt += 4);  SummFreq += 4;
        RunLength += PrevSuccess;
        if (HiCnt > MAX_FREQ)               rescale();
        SubRange.LowCount=0;                return;
    }
    i=NumStats;                             PrevSuccess=0;
    while ((HiCnt += (++p)->Freq) <= count)
        if (--i == 0) {
            if ( Suffix )                   PrefetchData(PPCTX(Suffix));
            SubRange.LowCount=HiCnt;        CharMask[p->Symbol]=EscCount;
            i=NumMasked=NumStats;           FoundState=NULL;
            do { CharMask[(--p)->Symbol]=EscCount; } while ( --i );
            SubRange.HighCount=SubRange.scale;
            return;
        }
    SubRange.LowCount=(SubRange.HighCount=HiCnt)-p->Freq;
    update1(p);
}
inline void PPM_CONTEXT::update2(STATE* p)
{
    (FoundState=p)->Freq += 4;              SummFreq += 4;
    if (p->Freq > MAX_FREQ)                 rescale();
    EscCount++;                             RunLength=InitRL;
}
inline SEE2_CONTEXT* PPM_CONTEXT::makeEscFreq2()
{
    BYTE* pb=(BYTE*)PPSTAT(Stats);          UINT t=2*NumStats;
    PrefetchData(pb);                       PrefetchData(pb+t);
    PrefetchData(pb += 2*t);                PrefetchData(pb+t);
    SEE2_CONTEXT* psee2c;
    if (NumStats != 0xFF) {
        t=PPCTX(Suffix)->NumStats;
        psee2c=SEE2Cont[QTable[NumStats+2]-3]+(SummFreq > 11*(NumStats+1));
        psee2c += 2*(2*NumStats < t+NumMasked)+Flags;
        SubRange.scale=psee2c->getMean();
    } else {
        psee2c=&DummySEE2Cont;              SubRange.scale=1;
    }
    return psee2c;
}
inline void PPM_CONTEXT::encodeSymbol2(int symbol)
{
    SEE2_CONTEXT* psee2c=makeEscFreq2();
    UINT Sym, LoCnt=0, i=NumStats-NumMasked;
    STATE* p1, * p=PPSTAT(Stats)-1;
    do {
        do { Sym=p[1].Symbol;   p++; } while (CharMask[Sym] == EscCount);
        CharMask[Sym]=EscCount;
        if (Sym == symbol)                  goto SYMBOL_FOUND;
        LoCnt += p->Freq;
    } while ( --i );
    SubRange.HighCount=(SubRange.scale += (SubRange.LowCount=LoCnt));
    psee2c->Summ += SubRange.scale;         NumMasked = NumStats;
    return;
SYMBOL_FOUND:
    SubRange.LowCount=LoCnt;                SubRange.HighCount=(LoCnt+=p->Freq);
    for (p1=p; --i ; ) {
        do { Sym=p1[1].Symbol;  p1++; } while (CharMask[Sym] == EscCount);
        LoCnt += p1->Freq;
    }
    SubRange.scale += LoCnt;
    psee2c->update();                       update2(p);
}
inline void PPM_CONTEXT::decodeSymbol2()
{
    SEE2_CONTEXT* psee2c=makeEscFreq2();
    UINT Sym, count, HiCnt=0, i=NumStats-NumMasked;
    STATE* ps[256], ** pps=ps, * p=PPSTAT(Stats)-1;
    do {
        do { Sym=p[1].Symbol;   p++; } while (CharMask[Sym] == EscCount);
        HiCnt += p->Freq;                   *pps++ = p;
    } while ( --i );
    SubRange.scale += HiCnt;                count=ariGetCurrentCount();
    p=*(pps=ps);
    if (count < HiCnt) {
        HiCnt=0;
        while ((HiCnt += p->Freq) <= count) p=*++pps;
        SubRange.LowCount = (SubRange.HighCount=HiCnt)-p->Freq;
        psee2c->update();                   update2(p);
    } else {
        SubRange.LowCount=HiCnt;            SubRange.HighCount=SubRange.scale;
        i=NumStats-NumMasked;               NumMasked = NumStats;
        do { CharMask[(*pps)->Symbol]=EscCount; pps++; } while ( --i );
        psee2c->Summ += SubRange.scale;
    }
}
inline void ClearMask(_PPMD_FILE* EncodedFile,_PPMD_FILE* DecodedFile)
{
    EscCount=1;                             memset(CharMask,0,sizeof(CharMask));
    if (++PrintCount == 0)                  PrintInfo(DecodedFile,EncodedFile);
}
void _STDCALL EncodeFile(_PPMD_FILE* EncodedFile,_PPMD_FILE* DecodedFile,
                            int MaxOrder,MR_METHOD MRMethod)
{
    ariInitEncoder();                       StartModelRare(MaxOrder,MRMethod);
    for (PPM_CONTEXT* MinContext; ; ) {
        BYTE ns=(MinContext=MaxContext)->NumStats;
        int c = _PPMD_E_GETC(DecodedFile);
        if ( ns ) {
            MinContext->encodeSymbol1(c);   ariEncodeSymbol();
        } else {
            MinContext->encodeBinSymbol(c); ariShiftEncodeSymbol(TOT_BITS);
        }
        while ( !FoundState ) {
            ARI_ENC_NORMALIZE(EncodedFile);
            do {
                OrderFall++;                MinContext=PPCTX(MinContext->Suffix);
                if ( !MinContext )          goto STOP_ENCODING;
            } while (MinContext->NumStats == NumMasked);
            MinContext->encodeSymbol2(c);   ariEncodeSymbol();
        }
        if (!OrderFall && GE_UNITS(FoundState->Successor))
                PrefetchData(MaxContext=PPCTX(FoundState->Successor));
        else {
            UpdateModel(MinContext);        PrefetchData(MaxContext);
            if (EscCount == 0)              ClearMask(EncodedFile,DecodedFile);
        }
        ARI_ENC_NORMALIZE(EncodedFile);
        if( _PPMD_ERROR_CODE(DecodedFile)<0 || _PPMD_ERROR_CODE(EncodedFile)<0 ) return;
    }
STOP_ENCODING:
    ARI_FLUSH_ENCODER(EncodedFile);         PrintInfo(DecodedFile,EncodedFile);
}
void _STDCALL DecodeFile(_PPMD_FILE* DecodedFile,_PPMD_FILE* EncodedFile,
                            int MaxOrder,MR_METHOD MRMethod)
{
    ARI_INIT_DECODER(EncodedFile);          StartModelRare(MaxOrder,MRMethod);
    PPM_CONTEXT* MinContext=MaxContext;
    for (BYTE ns=MinContext->NumStats; ; ) {
        ( ns )?(MinContext->decodeSymbol1()):(MinContext->decodeBinSymbol());
        ariRemoveSubrange();
        while ( !FoundState ) {
            ARI_DEC_NORMALIZE(EncodedFile);
            do {
                OrderFall++;                MinContext=PPCTX(MinContext->Suffix);
                if ( !MinContext )          goto STOP_DECODING;
            } while (MinContext->NumStats == NumMasked);
            MinContext->decodeSymbol2();    ariRemoveSubrange();
        }
        _PPMD_D_PUTC(FoundState->Symbol,DecodedFile);
        if (!OrderFall && GE_UNITS(FoundState->Successor))
                PrefetchData(MaxContext=PPCTX(FoundState->Successor));
        else {
            UpdateModel(MinContext);        PrefetchData(MaxContext);
            if (EscCount == 0)              ClearMask(EncodedFile,DecodedFile);
        }
        ns=(MinContext=MaxContext)->NumStats;
        ARI_DEC_NORMALIZE(EncodedFile);
        if( _PPMD_ERROR_CODE(DecodedFile)<0 || _PPMD_ERROR_CODE(EncodedFile)<0 ) return;
    }
STOP_DECODING:
    PrintInfo(DecodedFile,EncodedFile);
}
