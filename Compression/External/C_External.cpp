#include <stdio.h>
#include <string.h>
extern "C" {
#include "C_External.h"
}


int external_program (bool IsCompressing, CALLBACK_FUNC *callback, void *auxdata, char *infile, char *outfile, char *cmd, char *name, int MinCompression, double *addtime)
{
    BYTE* Buf = (BYTE*) BigAlloc(LARGE_BUFFER_SIZE);  // buffer used for reading/writing data
    if (!Buf)  {return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;}
    int x;                                            // code returned by the last read/write operation
    int ExitCode = 0;                                 // exit code of the external program
    bool useHeader = !strequ(name,"tempfile");        // TRUE if a 0/1 byte is written at the start of the compressed stream - data uncompressed/compressed

    // Copy the input data into a temporary file
    remove (infile);
    FILE *f = NULL;
    uint64 bytes = 0;
    BYTE runCmd = 1;
    if (!IsCompressing && useHeader)  checked_read (&runCmd, 1);
    while ( (x = callback ("read", Buf, LARGE_BUFFER_SIZE, auxdata)) > 0 )
    {
        if (f==NULL)  {f = fopen (infile, "wb");  // Don't open the file until we have read at least some data (to solve problems with recompressing solid blocks)
        	       if (!f)  {x=FREEARC_ERRCODE_IO; break;}
                       registerTemporaryFile (infile,f);}
        if (runCmd!=0 && runCmd!=1) {            // For compatibility with old FreeArc versions that did not prepend a 1 before the compressed data (remove in FreeArc 0.80!)
            outfile = "data7777";
            bytes += 1;
            if (file_write(f,&runCmd,1) != 1)   {x=FREEARC_ERRCODE_IO; break;}
            runCmd = 1;
        }
        bytes += x;
        if (file_write(f,Buf,x) != x)           {x=FREEARC_ERRCODE_IO; break;}
    }
    BigFree(Buf);  Buf = NULL;
    unregisterTemporaryFile (infile);
    fclose (f);    f = NULL;
    if (x)  {remove (infile); return x;}   // If an error occurred while reading/writing - exit

    // If cmd is empty - the disk is used simply to buffer the data before further compression.
    // If runCmd==0 - the data was copied without compression
    remove (outfile);
    registerTemporaryFile (infile);
    registerTemporaryFile (outfile);
    if (*cmd && runCmd) {
    	char temp[30];
        printf ("\n%s %s bytes with %s\n", IsCompressing? "Compressing":"Unpacking", show3(bytes,temp), cmd);
        double time0 = GetGlobalTime();
        ExitCode = system (cmd);
        printf ("\nErrorlevel=%d\n", ExitCode);
        if (addtime)  *addtime += GetGlobalTime() - time0;
    } else {
        rename (infile, outfile);
    }

    // Open the output file if the command finished successfully and the file can be opened
    if(ExitCode==0)    f = fopen (outfile, "rb" );
    if (f) {
        registerTemporaryFile (outfile,f);
        unregisterTemporaryFile (infile);
        remove (infile);
        BYTE compressed[1] = {1};
        if (IsCompressing && useHeader)     checked_write(compressed,1);
    } else {
        unregisterTemporaryFile (outfile);
        unregisterTemporaryFile (infile);
        if (IsCompressing && !useHeader)    {remove (infile); return FREEARC_ERRCODE_GENERAL;}
        remove (outfile);
        if (!IsCompressing)                 {remove (infile); return FREEARC_ERRCODE_INVALID_COMPRESSOR;}
        rename (infile, outfile);
        f = fopen (outfile, "rb" );
        if (!f)                             {remove (infile); remove (outfile); return FREEARC_ERRCODE_IO;}
        registerTemporaryFile (outfile,f);
        BYTE uncompressed[1] = {0};
        if (IsCompressing)                  checked_write(uncompressed,1);
    }

    // Read the output data from the file
    QUASIWRITE (get_flen(f));
    Buf = (BYTE*) BigAlloc(LARGE_BUFFER_SIZE);
    while ((x = file_read (f, Buf, LARGE_BUFFER_SIZE)) > 0)
    {
        checked_write (Buf, x);
    }
finished:
    unregisterTemporaryFile (outfile);
    fclose (f);
    remove (outfile);
    BigFree(Buf);
    return x;         // 0 if everything is fine, an error code otherwise
}


/*-------------------------------------------------*/
/* Implementation of the EXTERNAL_METHOD class     */
/*-------------------------------------------------*/

char *prepare_cmd (EXTERNAL_METHOD *p, char *cmd)
{
    // Replace "{options}" or "{-option }" in packcmd with string like "-m48 -r1 " (for "pmm:m48:r1" method string)
    char *OPTIONS_STR = "{options}",  *OPTION_STR = "option";
    char OPTIONS_START = '{',  OPTIONS_END = '}';

    // Params of option template in cmd line
    char before[MAX_METHOD_STRLEN] = "-";
    char after[MAX_METHOD_STRLEN]  =  " ";
    char *replaced = strstr (cmd, OPTIONS_STR);
    int  how_many  = strlen (OPTIONS_STR);

    // If there is no "{options}" in cmd - look for "{...option...}"
    if (!replaced)
    {
        // search for '{'
        for (char *p1 = cmd; *p1; p1++)
        {
            if (*p1 == OPTIONS_START)
            {
                // search for '}'
                char *p2 = p1, *p12 = NULL;
                for (; *p2; p2++)
                {
                    if (*p2 == OPTIONS_END)  break;
                    if (start_with(p2, OPTION_STR))  p12 = p2;
                }
                // if we have "option" inside of "{...}"
                if (*p2==OPTIONS_END && p12)
                {
                    // Save strings before and after "option" and how many chars in cmd to replace
                    strncopy (before, p1+1, p12-p1-1 + 1);
                    strncopy (after,  p12+strlen(OPTION_STR), p2-p12-strlen(OPTION_STR) + 1);
                    replaced = p1;
                    how_many = p2-p1+1;
                    break;
                }
            }
        }
    }

    // If we found any option template in cmd
    if (replaced)
    {
        // Collect in param_str options in cmd format
        char param_str[MAX_METHOD_STRLEN] = "";
        for (char **opt = p->options; *opt; opt++)
        {
            strcat (param_str, before);
            strcat (param_str, *opt);
            strcat (param_str, after);
        }
        // Finally replace template with collected or default options
        cmd = str_replace_n (cmd, replaced, how_many, *p->options? param_str : p->defaultopt);
    }

    return cmd;
}


// Decompression function
int EXTERNAL_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
    char *cmd = prepare_cmd (this, unpackcmd);
    int result = external_program (FALSE, callback, auxdata, packedfile, datafile, cmd, name, 0, &addtime);
    if (cmd != unpackcmd)  delete cmd;
    return result;
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Compression function
int EXTERNAL_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
    char *cmd = prepare_cmd (this, packcmd);
    int result = external_program (TRUE, callback, auxdata, datafile, packedfile, cmd, name, 0, &addtime);
    if (cmd != packcmd)  delete cmd;
    return result;
}

// Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_EXTERNAL)
void EXTERNAL_METHOD::ShowCompressionMethod (char *buf)
{
    if (strequ (name, "pmm")) {
        char MemStr[100];
        showMem (cmem, MemStr);
        sprintf (buf, "pmm:%d:%s%s", order, MemStr, MRMethod==2? ":r2": (MRMethod==0? ":r0":""));
    } else {
        strcpy (buf, name);
        for (char** opt=options; *opt; opt++)
        {
            strcat(buf, ":");
            strcat(buf, *opt);
        }
    }
}

// Change the memory requirements, tuning order along the way
void EXTERNAL_METHOD::SetCompressionMem (MemSize _mem)
{
    if (can_set_mem && _mem>0) {
        order  +=  int (trunc (log(double(_mem)/cmem) / log(2) * 4));
        cmem=dmem=_mem;
    }
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


// Constructs an object of type EXTERNAL_METHOD/PPMonstr with the given compression parameters
// or returns NULL if this is a different compression method or an error was made in the parameters
COMPRESSION_METHOD* parse_PPMONSTR (char** parameters)
{
  // If the method name (parameter zero) is "pmm", then parse the remaining parameters
  if (strcmp (parameters[0], "pmm") == 0) {
    // Default parameter values for the PPMonstr compression method
    EXTERNAL_METHOD *p = new EXTERNAL_METHOD;
    p->name           = "pmm";
    p->MinCompression = 100;
    p->can_set_mem    = TRUE;
    p->order          = 16;
    p->cmem           = 192*mb;
    p->dmem           = 192*mb;
    p->MRMethod       = 1;
    p->datafile       = "$$arcdatafile$$.tmp";
    p->packedfile     = "$$arcdatafile$$.pmm";

    int error = 0;  // Flag indicating that an error occurred while parsing the parameters

    // Iterate over all parameters of the method (or exit early if an error occurs while parsing one of them)
    while (*++parameters && !error)
    {
      char *param = *parameters;
      if (start_with (param, "mem")) {
        param+=2;  // Treat "mem..." as "m..."
      }
      if (strlen(param)==1) switch (*param) {    // Single-letter parameters
        case 'r':  p->MRMethod = 1; continue;
      }
      else switch (*param) {                    // Parameters carrying a value
        case 'm':  p->cmem = p->dmem = parseMem (param+1, &error); continue;
        case 'o':  p->order          = parseInt (param+1, &error); continue;
        case 'r':  p->MRMethod       = parseInt (param+1, &error); continue;
      }
      // We get here if the parameter does not specify its name
      // If this parameter can be parsed as an integer (i.e. it contains only digits),
      // then assign its value to the order field, otherwise try to parse it as mem
      int n = parseInt (param, &error);
      if (!error) p->order = n;
      else        error=0, p->cmem = p->dmem = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // Error while parsing the method parameters

    // Builds packcmd/unpackcmd for PPMonstr
    char cmd[100];
    sprintf (cmd, "ppmonstr e -o%d -m%d -r%d %s", p->order, p->cmem>>20, p->MRMethod, p->datafile);
    p->packcmd = strdup_msg(cmd);
    sprintf (cmd, "ppmonstr d %s", p->packedfile);
    p->unpackcmd = strdup_msg(cmd);

    return p;
  } else {
    return NULL;   // This is not the PPMONSTR method
  }
}

static int PPMONSTR_x = AddCompressionMethod (parse_PPMONSTR);   // Register the parser for the PPMONSTR method




// SUPPORT FOR ARBITRARY EXTERNAL COMPRESSORS **********************************************************************

// Constructs an object of type EXTERNAL_METHOD with the given compression parameters
// or returns NULL if this is a different compression method or an error was made in the parameters
COMPRESSION_METHOD* parse_EXTERNAL (char** parameters, void *method_template)
{
  if (strequ (parameters[0], ((EXTERNAL_METHOD*)method_template)->name)) {
    // If the method name (parameter zero) matches the name of the EXTERNAL method being checked, then parse the remaining parameters
    EXTERNAL_METHOD *p = new EXTERNAL_METHOD (*(EXTERNAL_METHOD*)method_template);

    // Copy the method parameters into our object
    char **param = parameters+1, **opt = p->options, *place = p->option_strings;
    while (*param)
    {
      strcpy (place, *param++);
      *opt++ = place;
      place += strlen(place)+1;
    }
    *opt++ = NULL;

    return p;
  } else {
    return NULL;   // This is not the EXTERNAL method
  }
}


// Add to the table of compression methods an external compressor described by the user in arc.ini.
// params contains the compressor description from arc.ini. Returns 1 if the description is valid.
// Example of a description:
//   [External compressor: ccm123, ccmx123, ccm125, ccmx125]
//   mem = 276
//   packcmd   = {compressor} c $$arcdatafile$$.tmp $$arcpackedfile$$.tmp
//   unpackcmd = {compressor} d $$arcpackedfile$$.tmp $$arcdatafile$$.tmp
//   datafile   = $$arcdatafile$$.tmp
//   packedfile = $$arcpackedfile$$.tmp
//
int AddExternalCompressor (char *params)
{
    // Split the compression method description into separate lines holding its header and parameters
    char  local_method [MAX_EXTERNAL_COMPRESSOR_SECTION_LENGTH];
    strncopy (local_method, params, MAX_METHOD_STRLEN);
    char* parameters [MAX_PARAMETERS];
    split (local_method, '\n', parameters, MAX_PARAMETERS);

    // Check that the first line is the header of an [External compressor] section
    if (last_char(parameters[0])=='\r')  last_char(parameters[0]) = '\0';
    if (! (start_with (parameters[0], "[External compressor:")
           && end_with (parameters[0], "]")))
      return 0;

    // Extract the names of the program versions from the section header
    char *versions_list = strdup_msg (strchr(parameters[0],':')+1);
    last_char(versions_list) = '\0';
    char* version_name [MAX_COMPRESSION_METHODS];
    int versions_count = split (versions_list, ',', version_name, MAX_COMPRESSION_METHODS);

    // For each version we create a separate EXTERNAL_METHOD object
    EXTERNAL_METHOD *version  =  new EXTERNAL_METHOD[versions_count];
    for (int i=0; i<versions_count; i++) {
        // Initialize the EXTERNAL_METHOD template with the name of the current version and default parameters
        version[i].name           = trim_spaces(version_name[i]);
        version[i].MinCompression = 100;
        version[i].can_set_mem    = FALSE;
        version[i].cmem           = 0;
        version[i].dmem           = 0;
        version[i].datafile       = "$$arcdatafile$$.tmp";
        version[i].packedfile     = "$$arcpackedfile$$.tmp";
        version[i].packcmd        = "";
        version[i].unpackcmd      = "";
        version[i].defaultopt     = "";
    }


    // Now fill in these templates from the compressor description supplied by the user
    // (compression/decompression commands, memory requirements and so on).
    for (char **param=parameters;  *++param; ) {
        // Process the description line, splitting it into the left part before '='
        // holding the parameter name and the right part holding its value
        char *s = *param;
        if (last_char(s)=='\r')  last_char(s) = '\0';  // In case a file with '\r\n' separators is being processed
        if (*s=='\0' || *s==';')  continue;  // Skip a completely empty line / a comment line
        while (*s && isspace(*s))  s++;   // Skip the leading spaces in the line
        char *left = s;                   // Anchor the start of the left part (the name) of the parameter
        while (*s && !isspace(*s) && *s!='=')  s++;   // Find the end of the name
        if (*s=='\0')  return 0;
        if (*s!='=') {                         // Skip the spaces after the name, if needed
            *s++ = '\0';
            while (*s && isspace(*s))  s++;
            if (*s!='=')  return 0;
        }
        *s++ = '\0';                           // Put a '\0' after the name
        while (*s && isspace(*s))  s++;        // Skip the spaces at the start of the right part (the value)
        if (*s=='\0')  return 0;
        char *right = s;                       // Anchor the start of the value

        // Now left holds the left part of the line (before '=') without spaces,
        // and right holds the right part without leading spaces.
        // Iterate over all compressor versions and update the corresponding field in each of them
        for (int i=0; i<versions_count; i++) {
            int error = 0;  // Flag indicating that an error occurred while parsing the parameters
                 if (strequ (left, "mem"))         version[i].cmem = version[i].dmem = parseInt (right,&error)*mb;
            else if (strequ (left, "cmem"))        version[i].cmem        = parseInt (right,&error)*mb;
            else if (strequ (left, "dmem"))        version[i].dmem        = parseInt (right,&error)*mb;
            else if (strequ (left, "packcmd"))     version[i].packcmd     = subst (strdup_msg(right), "{compressor}", version[i].name);
            else if (strequ (left, "unpackcmd"))   version[i].unpackcmd   = subst (strdup_msg(right), "{compressor}", version[i].name);
            else if (strequ (left, "datafile"))    version[i].datafile    = subst (strdup_msg(right), "{compressor}", version[i].name);
            else if (strequ (left, "packedfile"))  version[i].packedfile  = subst (strdup_msg(right), "{compressor}", version[i].name);
            else if (strequ (left, "default"))     version[i].defaultopt  = subst (strdup_msg(right), "{compressor}", version[i].name);
            else                                   error=1;

            if (error)  return 0;
        }
    }


    // Finally, register the parser for the EXTERNAL compression method, which uses these templates
    // to recognize new compression methods and to obtain all the information needed
    // about which commands must be invoked to implement it, through which files
    // the data is passed, and so on.
    for (int i=0; i<versions_count; i++) {
        AddExternalCompressionMethod (parse_EXTERNAL, &version[i]);
    }
    return 1;
}

// A pseudo compression method that writes all the data it receives to a file and then reads it back.
// Automatically inserted between memory-hungry algorithms, for example REP and LZMA
static int TEMPFILE_x = AddExternalCompressor ("[External compressor:tempfile]");   // Register the parser for the TEMPFILE method

