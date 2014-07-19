/*
 * $Id: ishftc.c,v 1.1.1.1 1996/02/15 17:50:00 mclareni Exp $
 *
 * $Log: ishftc.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:00  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* ishftc.f -- translated by f2c and been corrected by V.E.Fine by hand
*/

#include "kerngen/qf_f2c.h"

integer ishftc_(i, k, l)
integer *i, *k, *l;
{
    /* System generated locals */
    integer ret_val, i__1, i__2, i__3, i__4, i__5;


/* CERN PROGLIB# M441    ISHFTC          .VERSION KERNFOR  4.16  870601 */

/* ORIG. 01/10/85  T.LINDELOF */

/*      FORTRAN VERSION OF MIL-STD-1753 ISHFTC FUNCTION */
/*      K IS THE SHIFT COUNT - NEGATIVE TO THE RIGHT */
/*                             POSITIVE TO THE LEFT */
/*      L IS THE NUMBER OF THE RIGHTMOST BITS TO BE SHIFTED, ALL */
/*         OTHER BITS ARE LEFT UNCHANGED */


    if (*k > 0) {
        i__1 = -1 << *l;
  i__2 = (unsigned) (*i << 32 - *l)      >> 32 - *k;
  i__3 = (unsigned) (*i << 32 - *l + *k) >> 32 - *l;
        i__4 = i__1 & *i;
        i__5 = i__2 | i__3;
        ret_val = i__4 | i__5;
    } else if (*k < 0) {
        i__1 = -1 << *l;
  i__2 = (unsigned) (*i << 32 - *l) >> 32 - *l - *k;
  i__3 = (unsigned) (*i << *k + 32) >> 32 - *l;
        i__4 = i__1 & *i;
        i__5 = i__2 | i__3;
        ret_val = i__4 | i__5;
    } else {
        ret_val = *i;
    }
    return ret_val;
} /* ishftc_ */
#endif
