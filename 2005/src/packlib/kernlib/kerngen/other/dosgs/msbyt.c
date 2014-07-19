/*
 * $Id: msbyt.c,v 1.1.1.1 1996/02/15 17:50:04 mclareni Exp $
 *
 * $Log: msbyt.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:04  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* msbyt.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

integer msbyt_(mz, izw, izp, nzb)
integer *mz, *izw, *izp, *nzb;
{
    /* System generated locals */
    integer ret_val, i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8, i__9;


/* CERN PROGLIB# M421    MSBYT           .VERSION KERNFOR  4.23  891215 */

/* ORIG. 13/03/89  JZ */

/*     This non-ANSI code is a default which may be slow, if so */
/*     it should be replaced by a machine-specific fast routine */
    i__1 = 32 - *nzb;
    i__2 = (unsigned) (-1) >> i__1;
    i__3 = *izp - 1;
    i__4 = ~ (i__2 << i__3);
    i__5 = 32 - *nzb;
    i__6 = *mz << i__5;
    i__7 = 33 - *izp - *nzb;
    i__8 = *izw & i__4;
    i__9 = (unsigned) (i__6) >> i__7;
    ret_val = i__8 | i__9;
    return ret_val;
} /* msbyt_ */

#endif
