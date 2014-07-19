/*
 * $Id: intarg.c,v 1.1.1.1 1996/02/15 17:50:02 mclareni Exp $
 *
 * $Log: intarg.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:02  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* intarg.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

/* Table of constant values */

static integer c__24 = 24;
static integer c__9 = 9;

integer intarg_(argm)
real *argm;
{
    /* System generated locals */
    integer ret_val;
    static real equiv_0[1];

    /* Local variables */
#define a (equiv_0)
#define ia ((integer *)equiv_0)
    static integer jch;


/* CERN PROGLIB# M250    INTARG          .VERSION KERNFOR  4.23  891215 */

/* ORIG. 01/8/82  ADAPTED FROM UFLINT */






    /* Parameter adjustments */
    --argm;

    /* Function Body */
    *a = argm[1];
    jch = (unsigned) (*ia << 33 - c__24 - c__9) >> 32 - c__9;
    if (jch == 0) {
        goto L36;
    }
    if (jch == 511) {
        goto L36;
    }
    ret_val = *a;
    return ret_val;

L36:
    ret_val = *ia;
    return ret_val;
} /* intarg_ */

#undef ia
#undef a


#endif
