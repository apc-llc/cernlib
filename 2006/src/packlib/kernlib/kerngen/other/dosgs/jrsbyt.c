/*
 * $Id: jrsbyt.c,v 1.1.1.1 1996/02/15 17:50:04 mclareni Exp $
 *
 * $Log: jrsbyt.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:04  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* jrsbyt.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

integer jrsbyt_(val, x, jth, nbits)
integer *val, *x, *jth, *nbits;
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    extern /* Subroutine */ int sbyt_();


/* CERN PROGLIB# M421    JRSBYT          .VERSION KERNFOR  4.07  830624 */

/* ORIG. 03/05/72 JZ */

    ret_val = (unsigned) (*x << 33 - *jth - *nbits) >> 32 - *nbits;
    sbyt_(val, x, jth, nbits);
    return ret_val;
} /* jrsbyt_ */

#endif
