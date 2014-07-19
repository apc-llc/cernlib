/*
 * $Id: jbytpk.c,v 1.1.1.1 1996/02/15 17:50:04 mclareni Exp $
 *
 * $Log: jbytpk.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:04  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* jbytpk.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

integer jbytpk_(a, jth, nbits)
integer *a, *jth, *nbits;
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer nbit, jpos, jword, inword, loc;


/* CERN PROGLIB# M422    JBYTPK          .VERSION KERNFOR  4.07  830624 */

/* ORIG. 10/02/71 CL */

    /* Parameter adjustments */
    --nbits;
    --a;

    /* Function Body */
    nbit = nbits[1];
    if (nbit > 0) {
        goto L7;
    }
    nbit = 1;
    inword = 32;
    goto L11;

L7:
    inword = nbits[2];
L11:
    jword = (*jth - 1) / inword + 1;
    jpos = *jth - (jword - 1) * inword;
    loc = (jpos - 1) * nbit + 1;
    ret_val = (unsigned) (a[jword] << 33 - loc - nbit) >> 32 - nbit;
    return ret_val;
} /* jbytpk_ */

#endif
