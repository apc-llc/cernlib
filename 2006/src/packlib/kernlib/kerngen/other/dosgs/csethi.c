/*
 * $Id: csethi.c,v 1.1.1.1 1996/02/15 17:50:02 mclareni Exp $
 *
 * $Log: csethi.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:02  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)


/* csethi.f -- translated by f2c and  been corrected by V.E.Fine  by hand
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

static integer c__15 = 15;
static integer c__4 = 4;

/* Subroutine */ int csethi_(intp, chv, jlp, jrp, chv_len)
integer *intp;
char *chv;
integer *jlp, *jrp;
ftnlen chv_len;
{
    static integer ival, k, jj, jl, ndg;


/* CERN PROGLIB# M432    CSETHI          .VERSION KERNFOR  4.31  911111 */

/* ORIG. 17/10/89, JZ */

/* -    Set hexadecimal integer into CHV(JL:JR) right-justified */
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
    k = ival & c__15;
    ival = (unsigned) (ival) >> c__4;
    if (k < 10) {
        chv[jj - 1] = (k + 48);
    } else {
        chv[jj - 1] = (k + 55);
    }
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
} /* csethi_ */

#endif
