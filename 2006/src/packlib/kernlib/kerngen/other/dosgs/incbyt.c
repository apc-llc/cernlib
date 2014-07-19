/*
 * $Id: incbyt.c,v 1.1.1.1 1996/02/15 17:50:02 mclareni Exp $
 *
 * $Log: incbyt.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:02  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* incbyt.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

integer incbyt_(inc, a, jth, nbits)
integer *inc, *a, *jth, *nbits;
{
    /* Local variables */
    static integer jpos;
    extern /* Subroutine */ int sbyt_();
    static integer jword, loc, ict;


/* CERN PROGLIB# M423    INCBYT          .VERSION KERNFOR  4.07  830624 */

/* ORIG. 10/02/71 CL */




    /* Parameter adjustments */
    --nbits;
    --a;

    /* Function Body */
    jword = (*jth - 1) / nbits[2] + 1;
    jpos = *jth - (jword - 1) * nbits[2];
    loc = (jpos - 1) * nbits[1] + 1;
    ict = *inc + ((unsigned) (a[jword] << 33 - loc - nbits[1]) >>
          32 - nbits[1]);
    if (ict > nbits[3]) {
        goto L2;
    }
    sbyt_(&ict, &a[jword], &loc, &nbits[1]);
    return 0;

L2:
    sbyt_(&nbits[3], &a[jword], &loc, &nbits[1]);
    return ( ict - nbits[3]);
} /* incbyt_ */

#endif
