/*
 * $Id: sbyt.c,v 1.1.1.1 1996/02/15 17:50:04 mclareni Exp $
 *
 * $Log: sbyt.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:04  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* sbyt.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

/* Subroutine */ int sbyt_(it, izw, izp, nzb)
integer *it, *izw, *izp, *nzb;
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6;

    /* Local variables */
    static integer msk;


/* CERN PROGLIB# M421    SBYT            .VERSION KERNFOR  4.23  891215 */

/* MOD. true default 24/2/89, JZ */

/*     This non-ANSI code is a default which may be slow, if so */
/*     it should be replaced by a machine-specific fast routine */

    i__1 = 32 - *nzb;
    msk = (unsigned) (-1) >> i__1;
    i__1 = *izp - 1;
    i__2 = ~ (msk << i__1);
    i__3 = *it & msk;
    i__4 = *izp - 1;
    i__5 = *izw & i__2;
    i__6 = i__3 << i__4;
    *izw = i__5 | i__6;
    return 0;
} /* sbyt_ */

#endif
