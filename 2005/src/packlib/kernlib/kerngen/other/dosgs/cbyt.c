/*
 * $Id: cbyt.c,v 1.1.1.1 1996/02/15 17:50:02 mclareni Exp $
 *
 * $Log: cbyt.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:02  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* cbyt.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

/* Subroutine */ int cbyt_(m, lm, ix, lx, nb)
integer *m, *lm, *ix, *lx, *nb;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int sbyt_();


/* CERN PROGLIB# M421    CBYT            .VERSION KERNFOR  4.23  891215 */

/* ORIG. 14/08/70 CL */


    i__1 = (unsigned) (*m << 33 - *lm - *nb) >> 32 - *nb;
    sbyt_(&i__1, ix, lx, nb);
    return 0;
} /* cbyt_ */

#endif
