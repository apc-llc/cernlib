/*
 * $Id: upkbyt.c,v 1.1.1.1 1996/02/15 17:50:04 mclareni Exp $
 *
 * $Log: upkbyt.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:04  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
#include "kerngen/qf_f2c.h"

/* Subroutine */ int upkbyt_(mbv, jthp, miv, nintp, nbits)
integer *mbv, *jthp, *miv, *nintp, *nbits;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer mska, jive, nint, npwd, jpos, nr, jbv, jth, jiv, nzb, izw;


/* CERN PROGLIB# M422    UPKBYT          .VERSION KERNFOR  4.23  891215 */

/* ORIG. 13/03/89  JZ */

/*     This non-ANSI code is a default which may be slow, if so */
/*     it should be replaced by a machine-specific fast routine */

    /* Parameter adjustments */
    --nbits;
    --nintp;
    --miv;
    --jthp;
    --mbv;

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
    nr = jpos * nzb;
    jive = npwd - jpos;
    izw = (unsigned) (mbv[jbv]) >> nr;
    goto L22;
/* --                UNPACK EACH WORD */
L21:
    jive = jiv + npwd;
    izw = mbv[jbv];
L22:
    jive = min(nint,jive);
L24:
    ++jiv;
    miv[jiv] = mska & izw;
    if (jiv == jive) {
        goto L27;
    }
    izw = (unsigned) (izw) >> nzb;
    goto L24;
L27:
    if (jiv == nint) {
        return 0;
    }
    ++jbv;
    goto L21;
} /* upkbyt_ */

#endif
