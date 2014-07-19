/*
 * $Id: floarg.c,v 1.1.1.1 1996/02/15 17:50:02 mclareni Exp $
 *
 * $Log: floarg.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:02  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* floarg.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

/* Table of constant values */

static integer c__24 = 24;
static integer c__9 = 9;

doublereal floarg_(argm)
real *argm;
{
    /* System generated locals */
    static real equiv_0[1];

    /* Local variables */
#define a (equiv_0)
#define ia ((integer *)equiv_0)
    static integer jch;


/* CERN PROGLIB# M250    FLOARG          .VERSION KERNFOR  4.23  891215 */

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
    return( *a);

L36:
    return( (real) (*ia));
} /* floarg_ */

#undef ia
#undef a


#endif
