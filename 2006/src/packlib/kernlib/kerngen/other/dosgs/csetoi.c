/*
 * $Id: csetoi.c,v 1.1.1.1 1996/02/15 17:50:02 mclareni Exp $
 *
 * $Log: csetoi.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:02  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* csetoi.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

/* Common Block Declarations */

struct {
    integer ndslat, neslat, nfslat, ngslat;
    real dummy[36];
} slate_;

#define slate_1 slate_

/* Table of constant values */

static integer c__7 = 7;
static integer c__3 = 3;

/* Subroutine */ int csetoi_(intp, chv, jlp, jrp, chv_len)
integer *intp;
char *chv;
integer *jlp, *jrp;
ftnlen chv_len;
{
    static integer ival, k, jj, jl, ndg;


/* CERN PROGLIB# M432    CSETOI          .VERSION KERNFOR  4.31  911111 */

/* ORIG. 03/06/92, JZ */

/* -    Set octal integer into CHV(JL:JR) right-justified */
    /* Parameter adjustments */
    --jrp;
    --jlp;
    --intp;

    /* Function Body */
    jl = jlp[1];
    jj = jrp[1];
    ival = intp[1];
    ndg = 0;
    slate_1.ngslat = 0;
L12:
    if (jj < jl) {
        goto L97;
    }
    k = ival & c__7;
    ival = (unsigned) (ival) >> c__3;
    chv[jj - 1] = (k + 48);
    --jj;
    ++ndg;
    if (ival != 0) {
        goto L12;
    }
    goto L98;
L97:
    slate_1.ngslat = jl;
L98:
    slate_1.nfslat = jj;
    slate_1.neslat = jj;
    slate_1.ndslat = ndg;
    return 0;
} /* csetoi_ */

#endif
