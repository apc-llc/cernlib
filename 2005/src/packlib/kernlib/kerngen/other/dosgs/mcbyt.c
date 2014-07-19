/*
 * $Id: mcbyt.c,v 1.1.1.1 1996/02/15 17:50:04 mclareni Exp $
 *
 * $Log: mcbyt.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:04  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* mcbyt.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

integer mcbyt_(mz, izq, izw, izp, nzb)
integer *mz, *izq, *izw, *izp, *nzb;
{
    /* System generated locals */
    integer ret_val, i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8, i__9,
            i__10, i__11;


/* CERN PROGLIB# M421    MCBYT           .VERSION KERNFOR  4.23  891215 */

/* ORIG. 13/03/89  JZ */

/*     This non-ANSI code is a default which may be slow, if so */
/*     it should be replaced by a machine-specific fast routine */
    i__1 = 32 - *nzb;
    i__2 = (unsigned) (-1) >> i__1;
    i__3 = *izp - 1;
    i__4 = ~ (i__2 << i__3);
    i__5 = *izq - 1;
    i__6 = (unsigned) (*mz) >> i__5;
    i__7 = 32 - *nzb;
    i__8 = i__6 << i__7;
    i__9 = 33 - *izp - *nzb;
    i__10 = *izw & i__4;
    i__11 = (unsigned) (i__8) >> i__9;
    ret_val = i__10 | i__11;
    return ret_val;
} /* mcbyt_ */

#endif
