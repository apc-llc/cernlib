/*
 * $Id: iuweed.c,v 1.1.1.1 1996/02/15 17:50:02 mclareni Exp $
 *
 * $Log: iuweed.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:02  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* iuweed.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

/* Table of constant values */

static integer c__23 = 23;
static integer c__255 = 255;

integer iuweed_(ia, n)
integer *ia, *n;
{
    /* System generated locals */
    integer ret_val, i__1, i__2;

    /* Local variables */
    static integer ival, j;


/* CERN PROGLIB# N103    IUWEED          .VERSION KERNFOR  4.31  911111 */

/* ORIG. 16/10/91 JZ */
/* -    Find position of first number which is infinity or not-a-number */

/* -    in the N word vector IA, return zero if none */
/* -    ---  this version only for machines with IEEE representation  ---
*/
    /* Parameter adjustments */
    --ia;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
  i__2 = (unsigned) (ia[j]) >> c__23;
        ival = i__2 & c__255;
        if (ival == 255) {
            goto L29;
        }
/* L24: */
    }
    j = 0;
L29:
    ret_val = j;
    return ret_val;
} /* iuweed_ */

#endif
