/*
 * $Id: ubunch.c,v 1.1.1.1 1996/02/15 17:50:01 mclareni Exp $
 *
 * $Log: ubunch.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:01  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* ubunch.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

/* Subroutine */ int ubunch_(ms, mt, nchp)
integer *ms, *mt, *nchp;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int abend_();
    static integer js, jt, ntrail, nch, mwd, nwt;


/* CERN PROGLIB# M409    UBUNCH          .VERSION KERNDOS  1.00  920624 */

/* ORIG. 03/02/89 K.M.STORR */

    /* Parameter adjustments */
    --nchp;
    --mt;
    --ms;

    /* Function Body */
    nch = nchp[1];
    if (nch < 0) {
        goto L91;
    } else if (nch == 0) {
        goto L39;
    }
L11:
    nwt = (unsigned) (nch) >> 2;
    ntrail = nch & 3;
    js = 0;
    if (nwt == 0) {
        goto L31;
    }
/* --                Pack the initial complete words */
    i__1 = nwt;
    for (jt = 1; jt <= i__1; ++jt) {
        mt[jt] = ms[js + 1] & 255 | (ms[js + 2] & 255) << 8 | (ms[js + 3] &
                255) << 16 | ms[js + 4] << 24;
/* L24: */
        js += 4;
    }
    if (ntrail == 0) {
        return 0;
    }
/* --                Pack the trailing word */
L31:
    mwd = 538976288;
    js = nch;
    i__1 = ntrail;
    for (jt = 1; jt <= i__1; ++jt) {
        mwd = mwd << 8 | ms[js] & 255;
/* L34: */
        --js;
    }
    mt[nwt + 1] = mwd;
L39:
    return 0;
L91:
    abend_();
} /* ubunch_ */

#endif
