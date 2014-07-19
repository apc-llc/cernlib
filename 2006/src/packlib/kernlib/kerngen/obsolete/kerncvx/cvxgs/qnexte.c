/*
 * $Id: qnexte.c,v 1.1.1.1 1996/02/15 17:52:27 mclareni Exp $
 *
 * $Log: qnexte.c,v $
 * Revision 1.1.1.1  1996/02/15 17:52:27  mclareni
 * Kernlib
 *
 */
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
/*>    ROUTINE QNEXTE
*/
#include <setjmp.h>

static int      first = 0;
static jmp_buf  env;
int
qnexte_()
{
        int             val;
        if (first) {
                longjmp(env, val);
                first = 0;
        } else {
                setjmp(env);
                if (first)
                        return;
                else {
                        qnext_();
                        first = 1;
                }
        }
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_TCGEN_QNEXTE
#undef CERNLIB_TCGEN_QNEXTE
#endif
#ifdef CERNLIB_CCGEN_QNEXTE
#undef CERNLIB_CCGEN_QNEXTE
#endif
