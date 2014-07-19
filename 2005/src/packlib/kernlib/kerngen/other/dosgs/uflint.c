/*
 * $Id: uflint.c,v 1.1.1.1 1996/02/15 17:50:04 mclareni Exp $
 *
 * $Log: uflint.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:04  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* uflint.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__24 = 24;
static integer c__9 = 9;

/* Subroutine */ int uflint_(vect, nwp, modep)
real *vect;
integer *nwp, *modep;
{
    /* System generated locals */
    static real equiv_0[1], equiv_1[1];

    /* Local variables */
    static integer mode, juse;
    static logical vary;
#define a (equiv_0)
#define b (equiv_1)
    static integer n;
#define ia ((integer *)equiv_0)
#define ib ((integer *)equiv_1)
    static integer nw, jch;


/* CERN PROGLIB# M251    UFLINT          .VERSION KERNFOR  4.23  891215 */

/* ORIG. 01/10/77 JZ */

/* -    CONVERT VECT(1->NWP) IN SITU TO FLOATING OR INTEGERS ACCORDING */
/* -    TO MODEP */

/* -    PROGRAM WRITTEN 12-AUG-77, JZ */





    /* Parameter adjustments */
    --modep;
    --nwp;
    --vect;

    /* Function Body */
    nw = nwp[1];
    mode = modep[1];
    n = 1 - nw;
    vary = TRUE_;
    if (nw < 0) {
        goto L31;
    }
    n = nw + 1;
    vary = FALSE_;
    juse = (unsigned) (mode) >> c__1 - 1 & 1;
    goto L31;

L30:
    vect[n] = *b;
L31:
    --n;
    if (n == 0) {
        return 0;
    }
    if (vary) {
  juse = (unsigned) (mode) >> n - 1 & 1;
    }
    *a = vect[n];
    jch = (unsigned) (*ia << 33 - c__24 - c__9) >> 32 - c__9;
    if (jch == 0) {
        goto L36;
    }
    if (jch == 511) {
        goto L36;
    }

/* --                CONVERT TO INTEGER */

    if (juse == 0) {
        goto L31;
    }
    *ib = *a;
    goto L30;

/* --                CONVERT TO FLOATING */
L36:
    if (juse != 0) {
        goto L31;
    }
    *b = (real) (*ia);
    goto L30;
} /* uflint_ */

#undef ib
#undef ia
#undef b
#undef a


#endif
