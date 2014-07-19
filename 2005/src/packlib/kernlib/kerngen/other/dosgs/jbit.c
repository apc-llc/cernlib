/*
 * $Id: jbit.c,v 1.1.1.1 1996/02/15 17:50:02 mclareni Exp $
 *
 * $Log: jbit.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:02  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* jbit.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

/* Table of constant values */

static integer c__1 = 1;

integer jbit_(izw, izp)
integer *izw, *izp;
{
    /* System generated locals */
    integer ret_val, i__1, i__2;


/* CERN PROGLIB# M421    JBIT            .VERSION KERNFOR  4.23  891215 */

/* ORIG. 13/03/89  JZ */

/*     This non-ANSI code is a default which may be slow, if so */
/*     it should be replaced by a machine-specific fast routine */
    i__1 = *izp - 1;
    i__2 = (unsigned) (*izw) >> i__1;
    ret_val = i__2 & c__1;
    return ret_val;
} /* jbit_ */

#endif
