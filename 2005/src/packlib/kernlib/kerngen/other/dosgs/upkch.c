/*
 * $Id: upkch.c,v 1.1.1.1 1996/02/15 17:50:04 mclareni Exp $
 *
 * $Log: upkch.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:04  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
#include "kerngen/qf_f2c.h"

/* Common Block Declarations */

struct {
    integer jch, n, nbits, nchar, nzone, ignor, jin, nbdone, nbend, jgo, jbt,
            nleft, nrigh;
    real dummy[27];
} slate_;

#define slate_1 slate_

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int upkch_(ichv, igv, nn, ipar)
integer *ichv, *igv, *nn, *ipar;
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    extern /* Subroutine */ int cbyt_();


/* CERN PROGLIB# M427    UPKCH           .VERSION KERNFOR  4.20  881028 */

/* ORIG. 07/07/72 JZ */

/*                  . */

    /* Parameter adjustments */
    --ipar;
    --igv;
    --ichv;

    /* Function Body */
    slate_1.n = *nn;
    if (slate_1.n == 0) {
        return 0;
    }
    slate_1.nbits = ipar[1];
    slate_1.nchar = ipar[2];
    slate_1.nzone = ipar[3];
    slate_1.ignor = ipar[4];
    slate_1.jin = 1;
    slate_1.jch = 1;
    if (slate_1.nzone != 0) {
        goto L41;
    }

/* ----              ZONE = WORD */

L21:
    slate_1.jgo = 33 - slate_1.ignor;
    slate_1.nbend = slate_1.nbits + 1;
    if (slate_1.nchar == 0) {
        goto L22;
    }
/* Computing MAX */
    i__1 = slate_1.nbend, i__2 = slate_1.jgo - slate_1.nbits * (slate_1.nchar
            - 1);
    slate_1.nbend = max(i__1,i__2);

L22:
    slate_1.jbt = slate_1.jgo;

L24:
    slate_1.jbt -= slate_1.nbits;
    igv[slate_1.jin] =
        (unsigned) (ichv[slate_1.jch] << 33 - slate_1.jbt - slate_1.nbits)
            >> 32 - slate_1.nbits;
    if (slate_1.jin == slate_1.n) {
        return 0;
    }
    ++slate_1.jin;
    if (slate_1.jbt >= slate_1.nbend) {
        goto L24;
    }
    ++slate_1.jch;
    goto L22;

/* ----              ZONE NOT EQUALS A WORD */

L41:
    if (slate_1.nzone == 32) {
        goto L21;
    }
    slate_1.jbt = 32 - slate_1.ignor;
    slate_1.nbend = slate_1.nzone + 1 - slate_1.nbits;
    if (slate_1.nchar == 0) {
        goto L42;
    }
/* Computing MIN */
    i__1 = slate_1.nbend, i__2 = slate_1.ignor + slate_1.nchar *
            slate_1.nbits;
    slate_1.nbend = min(i__1,i__2);

L42:
    slate_1.nbdone = slate_1.ignor;
L43:
    if (slate_1.jbt >= 0) {
        goto L44;
    }
    slate_1.jbt += 32;
    ++slate_1.jch;
    goto L43;

L44:
    slate_1.jbt -= slate_1.nbits;
    if (slate_1.jbt < 0) {
        goto L51;
    }
    i__1 = slate_1.jbt + 1;
    igv[slate_1.jin] = (unsigned) (ichv[slate_1.jch]
                                  << 33 - i__1 - slate_1.nbits)
                                  >> 32 - slate_1.nbits;
    goto L58;

/* --                CHARACTER ACROSS WORD BOUNDARY */
L51:
    slate_1.nleft = slate_1.nbits + slate_1.jbt;
    slate_1.nrigh = -slate_1.jbt;
    slate_1.jbt += 32;
    i__1 = slate_1.jbt + 1;
    igv[slate_1.jin] = (unsigned)
                       (ichv[slate_1.jch + 1] << 33 - i__1 - slate_1.nrigh)
                       >> 32 - slate_1.nrigh;
    if (slate_1.nleft == 0) {
        goto L54;
    }
    i__1 = slate_1.nrigh + 1;
    cbyt_(&ichv[slate_1.jch], &c__1, &igv[slate_1.jin], &i__1, &slate_1.nleft)
            ;
L54:
    ++slate_1.jch;

L58:
    if (slate_1.jin == slate_1.n) {
        return 0;
    }
    ++slate_1.jin;
    slate_1.nbdone += slate_1.nbits;
    if (slate_1.nbdone < slate_1.nbend) {
        goto L44;
    }
    slate_1.jbt = slate_1.jbt - (slate_1.nzone - slate_1.nbdone) -
            slate_1.ignor;
    goto L42;
} /* upkch_ */
#endif
