/*
 * $Id: jbytet.c,v 1.1.1.1 1996/02/15 17:50:03 mclareni Exp $
 *
 * $Log: jbytet.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:03  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* jbytet.f -- translated by f2c and  been corrected by V.E.Fine  by hand
*/

#include "kerngen/qf_f2c.h"

integer jbytet_(mz, izw, izp, nzb)
integer *mz, *izw, *izp, *nzb;
{
    /* System generated locals */
    integer  i__1, i__2, i__3, i__4;


/* CERN PROGLIB# M421    JBYTET          .VERSION KERNFOR  4.23  891215 */

/* ORIG. 13/03/89  JZ */

/*     This non-ANSI code is a default which may be slow, if so */
/*     it should be replaced by a machine-specific fast routine */
    i__1 = 33 - *izp - *nzb;
    i__2 = *izw << i__1;
    i__3 = 32 - *nzb;
    i__4 = (unsigned) (i__2) >> i__3;
    return *mz & i__4;
} /* jbytet_ */

#endif
