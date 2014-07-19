/*
 * $Id: ublow.c,v 1.1.1.1 1996/02/15 17:50:01 mclareni Exp $
 *
 * $Log: ublow.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:01  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* ublow.f -- translated by f2c and  been corrected by V.E.Fine  by hand
*/

#include "kerngen/qf_f2c.h"

/* Subroutine */ int ublow_(ms, mt, nchp)
integer *ms, *mt, *nchp;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int abend_();
    static integer js, jt, ntrail, nch, mwd, nws;


/* CERN PROGLIB# M409    UBLOW           .VERSION KERNDOS  1.00  920624 */

/* ORIG. 03/02/89  K.M.STORR */

    /* Parameter adjustments */
    --nchp;
    --mt;
    --ms;

    /* Function Body */
    nch = nchp[1];
    if (nch < 0) {
        goto L91;
    } else if (nch == 0) {
        goto L29;
    }
L11:
    nws = (unsigned) (nch) >> 2;
    ntrail = nch & 3;
    jt = 0;
    if (nws == 0) {
        goto L26;
    }
/* --                Unpack the initial complete words */
    i__1 = nws;
    for (js = 1; js <= i__1; ++js) {
        mwd = ms[js];
  mt[jt + 1] = 538976256 | 255 & mwd;
  mt[jt + 2] = 538976256 | 255 & (unsigned) (mwd) >>  8;
  mt[jt + 3] = 538976256 | 255 & (unsigned) (mwd) >> 16;
  mt[jt + 4] = 538976256 |       (unsigned) (mwd) >> 24;
/* L24: */
        jt += 4;
    }
    if (ntrail == 0) {
        return 0;
    }
/* --                Unpack the trailing word */
L26:
    mwd = ms[nws + 1];
    i__1 = ntrail;
    for (js = 1; js <= i__1; ++js) {
        mt[jt + 1] = 538976256 | 255 & mwd;
  mwd = (unsigned) (mwd) >> 8;
/* L28: */
        ++jt;
    }
L29:
    return 0;
L91:
    abend_();
} /* ublow_ */

#endif
