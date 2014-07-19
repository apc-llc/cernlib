/*
 * $Id: sbytor.c,v 1.1.1.1 1996/02/15 17:50:04 mclareni Exp $
 *
 * $Log: sbytor.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:04  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* sbytor.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

/* Subroutine */ int sbytor_(it, izw, izp, nzb)
integer *it, *izw, *izp, *nzb;
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;


/* CERN PROGLIB# M421    SBYTOR          .VERSION KERNFOR  4.23  891215 */

/* ORIG. 13/03/89  JZ */

/*     This non-ANSI code is a default which may be slow, if so */
/*     it should be replaced by a machine-specific fast routine */

    i__1 = 32 - *nzb;
    i__2 = (unsigned) (-1) >> i__1;
    i__3 = *it & i__2;
    i__4 = *izp - 1;
    i__5 = i__3 << i__4;
    *izw |= i__5;
    return 0;
} /* sbytor_ */

#endif
