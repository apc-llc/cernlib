/*
 * $Id: pkbyt.c,v 1.1.1.1 1996/02/15 17:50:04 mclareni Exp $
 *
 * $Log: pkbyt.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:04  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* pkbyt.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

/* Subroutine */ int pkbyt_(miv, mbv, jthp, nintp, nbits)
integer *miv, *mbv, *jthp, *nintp, *nbits;
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

    /* Local variables */
    static integer mska, jive, nint, npwd, jpos, msku, nl, jbv, jth, jiv, nzb,
             izw;


/* CERN PROGLIB# M422    PKBYT           .VERSION KERNFOR  4.23  891215 */

/* ORIG. 13/03/89  JZ */

/*     This non-ANSI code is a default which may be slow, if so */
/*     it should be replaced by a machine-specific fast routine */

    /* Parameter adjustments */
    --nbits;
    --nintp;
    --jthp;
    --mbv;
    --miv;

    /* Function Body */
    jth = jthp[1];
    nint = nintp[1];
    if (nint <= 0) {
        return 0;
    }
    nzb = nbits[1];
    if (nzb > 0) {
        goto L11;
    }
    nzb = 1;
    npwd = 32;
    mska = 1;
    goto L12;
L11:
    npwd = nbits[2];
    i__1 = 32 - nzb;
    mska = (unsigned) (-1) >> i__1;
L12:
    jbv = 1;
    jiv = 0;
    if (jth < 2) {
        goto L21;
    }
    jbv = (jth - 1) / npwd + 1;
    jpos = jth - (jbv - 1) * npwd - 1;
    if (jpos == 0) {
        goto L21;
    }
    nl = jpos * nzb;
    msku = mska << nl;
    jive = npwd - jpos;
    goto L22;
/* --                PACK EACH WORD */
L21:
    nl = 0;
    msku = mska;
    jive = jiv + npwd;
L22:
    jive = min(nint,jive);
    izw = mbv[jbv];
L24:
    ++jiv;
    i__1 = ~ msku;
    i__2 = miv[jiv] << nl;
    i__3 = i__1 & izw;
    i__4 = msku & i__2;
    izw = i__3 | i__4;
    if (jiv == jive) {
        goto L27;
    }
    nl += nzb;
    msku <<= nzb;
    goto L24;
L27:
    mbv[jbv] = izw;
    if (jiv == nint) {
        return 0;
    }
    ++jbv;
    goto L21;
} /* pkbyt_ */

#endif
