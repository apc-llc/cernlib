/*
 * $Id: locbyt.c,v 1.1.1.1 1996/02/15 17:50:04 mclareni Exp $
 *
 * $Log: locbyt.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:04  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* locbyt.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

integer locbyt_(it, ia, n, nw, la, nbits)
integer *it, *ia, *n, *nw, *la, *nbits;
{
    /* System generated locals */
    integer ret_val, i__1, i__2;

    /* Local variables */
    static integer j;


/* CERN PROGLIB# M428    LOCBYT          .VERSION KERNFOR  4.07  830624 */

/* ORIG. 03/05/72 */

    /* Parameter adjustments */
    --ia;

    /* Function Body */
    if (*n == 0) {
        goto L19;
    }
    i__1 = *n;
    i__2 = *nw;
    for (j = 1; i__2 < 0 ? j >= i__1 : j <= i__1; j += i__2) {
  if ((unsigned) (ia[j] << 33 - *la - *nbits) >> 32 - *nbits == *it) {
            goto L21;
        }
/* L18: */
    }
L19:
    ret_val = 0;
    return ret_val;

L21:
    ret_val = j;
    return ret_val;
} /* locbyt_ */

#endif
