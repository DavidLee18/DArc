/*
** See Copyright Notice in lua.h
*/

#include <ctype.h>
#include <stdio.h>

#define luac_c
#define LUA_CORE

#include "ldebug.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lundump.h"

/* Weak fallback: overridden by GHC's foreign-export for hsmethod__call.
   Under MicroHs (no foreign export support), this stub is used instead. */
__attribute__((weak)) int hsmethod__call( lua_State *state ) { return -1; }

LUAI_FUNC int lua_neutralize_longjmp( lua_State *state )
{
    int result;
    result = hsmethod__call(state);
    if( result <0 )
        return lua_error(state);
    return result;
}
