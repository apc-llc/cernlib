/*
 * $Id: iumode.c,v 1.1.1.1 1996/02/15 17:50:02 mclareni Exp $
 *
 * $Log: iumode.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:02  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* iumode.f -- translated by f2c and  been corrected by V.E.Fine  by hand
*/

#include "kerngen/qf_f2c.h"

/* Table of constant values */

static integer c__24 = 24;
static integer c__9 = 9;

integer iumode_(word)
integer *word;
{
    /* Local variables */
    static integer j;


/* CERN PROGLIB# M506    IUMODE          .VERSION KERNFOR  4.23  891215 */

/* ORIG. 14/04/79 JZ */

/*     RETURN ZERO IF WORD CONTAINS AN INTEGER */
/*     PROGRAM WRITTEN 27-APR-79, JZ */

    j = (unsigned) (*word << 33 - c__24 - c__9) >> 32 - c__9;
    if (j == 511) {
        j = 0;
    }
    return (j);
} /* iumode_ */

#endif
