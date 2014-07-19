/*
 * $Id: ckrack.c,v 1.1.1.1 1996/02/15 17:50:02 mclareni Exp $
 *
 * $Log: ckrack.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:02  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
/* ckrack.f -- translated by f2c and  been corrected by V.E.Fine  by hand
   You must link the resulting object file with the libraries:
        -lF77 -lI77 -lm -lc   (in that order)
*/

#include "kerngen/qf_f2c.h"

/* Common Block Declarations */

struct {
    integer ndigt, neslat, mode, ngslat, num[2], ivalv[6], nexpv[6], jxop,
            jxme, jxfa, jxfe, jterm, iphase, negm, nege, nexpm, nerr;
    real dummy[12];
} slate_;

#define slate_1 slate_

/* Table of constant values */

static integer c__27 = 27;
static struct { integer fill; char val[8+1]; char fill2[3]; } c_b18_st = { 0,
        " .+-EDed" };
#define c_b18 c_b18_st.val
static integer c__2 = 2;
static integer c__1 = 1;
static struct { integer fill; char val[2+1]; char fill2[1]; } c_b38_st = { 0,
        "+-" };
#define c_b38 c_b38_st.val
static real c_b44 = (float)10.;
static doublereal c_b54 = 10.;
static struct { integer fill; char val[7+1]; } c_b60_st = { 0,"0OoBbXx" };
#define c_b60 c_b60_st.val
static integer c__3 = 3;
static integer c__4 = 4;

/* Subroutine */ int ckrack_(chv, jlp, jrp, iflagd, chv_len)
char *chv;
integer *jlp, *jrp, *iflagd;
ftnlen chv_len;
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_indx();
    double pow_ri(), pow_di();

    /* Local variables */
    static integer ndig, ival;
#define anum ((real *)&slate_1 + 4)
#define dnum ((doublereal *)&slate_1 + 2)
    static integer j, k, n, nexpu, jj, jr;
    static doublereal dfract;
#define islate ((integer *)&slate_1)
    static integer ntrail, ivalnt;


/* CERN PROGLIB# M432    CKRACK          .VERSION KERNFOR  4.29  910718 */

/* ORIG. 12/06/91, JZ */

/* -    Read the next number from CHV(JL:JR) */
/* -    formats: (1) bits     - #On octal, or #Bn binary, or #Xn hex */
/* -             (2) integer  - [+|-]n */
/* -             (3) floating - [+|-][n][.][f][E][+|-][n] */
/* -             (4) double   - [+|-][n][.][f]D[+|-][n] */
/* - */
/* -    Returns: NDSLAT   number of numeric digits seen */
/* -             NESLAT   COL(NESLAT) is the terminating character */
/* -             NFSLAT   mode: -ve bad, 0 blank, 1 B, 2 I, 3 F, 4 D */
/* -             NGSLAT   = zero if correct termination */
/* -                      = NESLAT otherwise */
/* -             NUM(1) or ANUM(1) or DNUM  returns the value */
    /* Parameter adjustments */
    --jrp;
    --jlp;
    --chv;

    /* Function Body */
    jj = jlp[1];
    jr = jrp[1];
    for (j = 1; j <= 28; ++j) {
/* L12: */
        islate[j - 1] = 0;
    }
/* ----              Look at the first character of the number */
L17:
    if (jj > jr) {
        goto L90;
    }
    if (chv[jj] == ' ') {
        ++jj;
        goto L17;
    } else if (chv[jj] == '#') {
        goto L71;
    } else if (chv[jj] == '+') {
        ++jj;
    } else if (chv[jj] == '-') {
        slate_1.negm = 7;
        ++jj;
    }
/* ----              Read an integer */
L21:
    slate_1.jterm = 0;
    ndig = 0;
    ival = 0;
L22:
    if (jj > jr) {
        goto L27;
    }
    k = chv[jj];
    k += -48;
    if (k < 0) {
        goto L26;
    }
    if (k >= 10) {
        goto L26;
    }
    if ((unsigned) (ival) >> c__27 != 0) {
        goto L24;
    }
L23:
    ++jj;
    ival = ival * 10 + k;
    ++ndig;
    goto L22;
/* --       getting near the integer capacity */
L24:
    if (slate_1.iphase != 0) {
        goto L93;
    }
    if (slate_1.jxop < 6) {
        ++slate_1.jxop;
        slate_1.ivalv[slate_1.jxop - 1] = ival;
        slate_1.nexpv[slate_1.jxop - 1] = ndig;
    }
    ival = 0;
    goto L23;
L26:
    slate_1.jterm = i_indx(c_b18, chv + jj, 8L, 1L) - 1;
/* -                     01234567 */
L27:
    if (ndig != 0) {
        slate_1.ndigt += ndig;
        if (slate_1.jxop < 6) {
            ++slate_1.jxop;
            slate_1.ivalv[slate_1.jxop - 1] = ival;
            slate_1.nexpv[slate_1.jxop - 1] = ndig;
        }
    }
/* --       IPHASE = 0 :  IVAL is the leading integer */
/* -                 2 :       is the exponent integer */
    if (slate_1.iphase != 0) {
        goto L51;
    }
    slate_1.jxme = slate_1.jxop;
    if (slate_1.jterm >= 1) {
        goto L31;
    }
/* --       pure integer */
L28:
    if (slate_1.ndigt == 0) {
        goto L91;
    }
    slate_1.mode = 2;
    slate_1.num[0] = slate_1.ivalv[0];
    if (slate_1.jxme < 2) {
        goto L29;
    }
    n = slate_1.nexpv[1] - slate_1.nexpv[0];
    if (n >= 2) {
        goto L92;
    }
    slate_1.ivalv[0] = (slate_1.num[0] << c__2) + slate_1.ivalv[0];
    slate_1.num[0] = slate_1.ivalv[0] << c__1;
    if (slate_1.num[0] < 0) {
        goto L92;
    }
    slate_1.num[0] += slate_1.ivalv[1];
    if (slate_1.num[0] < 0) {
        goto L92;
    }
L29:
    if (slate_1.negm != 0) {
        slate_1.num[0] = -slate_1.num[0];
    }
L30:
    slate_1.neslat = jj;
    if (slate_1.jterm == 0) {
        return 0;
    }
    slate_1.ngslat = jj;
    return 0;
/* ----              Read the fraction */
L31:
    if (slate_1.jterm != 1) {
        goto L41;
    }
    slate_1.jxfa = slate_1.jxop;
    slate_1.mode = 3;
    ++jj;
    slate_1.jterm = 0;
    ndig = 0;
L32:
    ival = 0;
    ivalnt = 0;
    ntrail = 0;
L33:
    if (jj > jr) {
        goto L37;
    }
    k = chv[jj];
    k += -48;
    if (k < 0) {
        goto L36;
    }
    if (k >= 10) {
        goto L36;
    }
    if ((unsigned) (ival) >> c__27 != 0) {
        goto L34;
    }
    ++jj;
    ival = ival * 10 + k;
    ++ndig;
    if (k == 0) {
        ++ntrail;
    } else {
        ntrail = 0;
        ivalnt = ival;
    }
    goto L33;
/* --       getting near the integer capacity */
L34:
    if (slate_1.jxop < 6) {
        ++slate_1.jxop;
        slate_1.ivalv[slate_1.jxop - 1] = ivalnt;
        slate_1.nexpv[slate_1.jxop - 1] = ntrail - ndig;
    }
    goto L32;
L36:
    slate_1.jterm = i_indx(c_b18, chv + jj, 8L, 1L) - 1;
/* -                     01234567 */
L37:
    slate_1.ndigt += ndig;
    if (ival != 0) {
        if (slate_1.jxop < 6) {
            ++slate_1.jxop;
            slate_1.ivalv[slate_1.jxop - 1] = ivalnt;
            slate_1.nexpv[slate_1.jxop - 1] = ntrail - ndig;
        }
    }
    slate_1.jxfe = slate_1.jxop;
    if (slate_1.jterm < 2) {
        goto L52;
    }
/* ----              Read the exponent */
L41:
    slate_1.iphase = 2;
    if (slate_1.ndigt == 0) {
        goto L91;
    }
    if (slate_1.jterm >= 4) {
        goto L44;
    }
    if (jj == jr) {
        if (slate_1.mode == 0) {
            goto L28;
        }
        goto L52;
    }
    slate_1.mode = 3;
    slate_1.nege = slate_1.jterm - 2;
    ++jj;
    goto L21;
L44:
    if (slate_1.jterm >= 6) {
        slate_1.jterm += -2;
    }
    slate_1.mode = slate_1.jterm - 1;
    ++jj;
    if (jj > jr) {
        slate_1.jterm = 0;
        goto L52;
    }
    j = i_indx(c_b38, chv + jj, 2L, 1L);
    if (j == 0) {
        goto L21;
    }
    if (jj == jr) {
        goto L52;
    }
    slate_1.nege = j - 1;
    ++jj;
    goto L21;
/* --                Exponent complete, construct the number */
L51:
    slate_1.nexpm = ival;
    if (slate_1.nege != 0) {
        slate_1.nexpm = -slate_1.nexpm;
    }
L52:
    if (*iflagd >= 0) {
        if (slate_1.mode == 4) {
            goto L61;
        }
        if (*iflagd != 0) {
            goto L61;
        }
    }
    anum[0] = (float)0.;
    anum[1] = (float)0.;
    if (slate_1.jxme == 0) {
        goto L56;
    }
/* --       single precision, integer part */
    anum[0] = (real) slate_1.ivalv[0];
    if (slate_1.jxme >= 2) {
        n = slate_1.nexpv[1] - slate_1.nexpv[0];
        i__1 = n;
        for (j = 1; j <= i__1; ++j) {
/* L53: */
            anum[0] *= (float)10.;
        }
        anum[0] += (real) slate_1.ivalv[1];
    }
    if (slate_1.nexpm == 0) {
        goto L56;
    }
    if (slate_1.nexpm < 0) {
        goto L55;
    }
    if (slate_1.nexpm > 9) {
        goto L55;
    }
    i__1 = slate_1.nexpm;
    for (j = 1; j <= i__1; ++j) {
/* L54: */
        anum[0] *= (float)10.;
    }
    goto L56;
L55:
    anum[0] *= pow_ri(&c_b44, &slate_1.nexpm);
/* --       single precision, fraction */
L56:
    if (slate_1.jxfe <= slate_1.jxfa) {
        goto L60;
    }
    i__1 = slate_1.jxfe;
    for (slate_1.jxop = slate_1.jxfa + 1; slate_1.jxop <= i__1;
            ++slate_1.jxop) {
        anum[1] = (real) slate_1.ivalv[slate_1.jxop - 1];
        nexpu = slate_1.nexpv[slate_1.jxop - 1] + slate_1.nexpm;
        if (nexpu == 0) {
            goto L59;
        }
        if (nexpu < 0) {
            goto L58;
        }
        if (nexpu > 9) {
            goto L58;
        }
        i__2 = nexpu;
        for (j = 1; j <= i__2; ++j) {
/* L57: */
            anum[1] *= (float)10.;
        }
        goto L59;
L58:
        anum[1] *= pow_ri(&c_b44, &nexpu);
L59:
        anum[0] += anum[1];
    }
L60:
    if (slate_1.negm != 0) {
        anum[0] = -(doublereal)anum[0];
    }
    goto L30;
/* --       double precision, integer part */
L61:
    slate_1.mode = 4;
    *dnum = (float)0.;
    if (slate_1.jxme == 0) {
        goto L66;
    }
    *dnum = (doublereal) slate_1.ivalv[0];
    if (slate_1.jxme >= 2) {
        n = slate_1.nexpv[1] - slate_1.nexpv[0];
        i__1 = n;
        for (j = 1; j <= i__1; ++j) {
/* L63: */
            *dnum *= (float)10.;
        }
        *dnum += (doublereal) slate_1.ivalv[1];
    }
    if (slate_1.nexpm == 0) {
        goto L66;
    }
    if (slate_1.nexpm < 0) {
        goto L65;
    }
    if (slate_1.nexpm > 9) {
        goto L65;
    }
    i__1 = slate_1.nexpm;
    for (j = 1; j <= i__1; ++j) {
/* L64: */
        *dnum *= (float)10.;
    }
    goto L66;
L65:
    *dnum *= pow_di(&c_b54, &slate_1.nexpm);
/* --       double precision, fraction */
L66:
    if (slate_1.jxfe <= slate_1.jxfa) {
        goto L70;
    }
    i__1 = slate_1.jxfe;
    for (slate_1.jxop = slate_1.jxfa + 1; slate_1.jxop <= i__1;
            ++slate_1.jxop) {
        dfract = (doublereal) slate_1.ivalv[slate_1.jxop - 1];
        nexpu = slate_1.nexpv[slate_1.jxop - 1] + slate_1.nexpm;
        if (nexpu == 0) {
            goto L69;
        }
        if (nexpu < 0) {
            goto L68;
        }
        if (nexpu > 9) {
            goto L68;
        }
        i__2 = nexpu;
        for (j = 1; j <= i__2; ++j) {
/* L67: */
            dfract *= (float)10.;
        }
        goto L69;
L68:
        dfract *= pow_di(&c_b54, &nexpu);
L69:
        *dnum += dfract;
    }
L70:
    if (slate_1.negm != 0) {
        *dnum = -(*dnum);
    }
    goto L30;
/* --------          Reading octal or binary or hexadecimal */
L71:
    j = i_indx(c_b60, chv + (jj + 1), 7L, 1L);
    if (j == 0) {
        goto L94;
    }
    jj += 2;
    if (j >= 6) {
        goto L82;
    }
    if (j >= 4) {
        goto L76;
    }
/* --       octal */
L72:
    if (jj > jr) {
        goto L87;
    }
    k = chv[jj];
    if (k == 32) {
        goto L87;
    }
    k += -48;
    if (k < 0) {
        goto L86;
    }
    if (k >= 8) {
        goto L86;
    }
    slate_1.num[0] = (slate_1.num[0] << c__3) + k;
    ++slate_1.ndigt;
    ++jj;
    goto L72;
/* --       binary */
L76:
    if (jj > jr) {
        goto L87;
    }
    k = chv[jj];
    if (k == 32) {
        goto L87;
    }
    k += -48;
    if (k < 0) {
        goto L86;
    }
    if (k >= 2) {
        goto L86;
    }
    slate_1.num[0] = (slate_1.num[0] << c__1) + k;
    ++slate_1.ndigt;
    ++jj;
    goto L76;
/* --       hexadecimal */
L82:
    if (jj > jr) {
        goto L87;
    }
    k = chv[jj];
    if (k == 32) {
        goto L87;
    }
    k += -48;
    if (k < 0) {
        goto L86;
    }
    if (k < 10) {
        goto L84;
    }
    k += -7;
    if (k < 8) {
        goto L86;
    }
    if (k < 16) {
        goto L84;
    }
    k += -32;
    if (k < 8) {
        goto L86;
    }
    if (k >= 16) {
        goto L86;
    }
L84:
    slate_1.num[0] = (slate_1.num[0] << c__4) + k;
    ++slate_1.ndigt;
    ++jj;
    goto L82;
L86:
    slate_1.jterm = -1;
L87:
    slate_1.mode = 1;
    if (slate_1.ndigt == 0) {
        goto L91;
    }
    goto L30;
/* ----              Special error exits */
L94:
    slate_1.nerr = -1;
L93:
    --slate_1.nerr;
L92:
    --slate_1.nerr;
L91:
    --slate_1.nerr;
L90:
    slate_1.mode = slate_1.nerr;
    slate_1.neslat = jj;
    slate_1.ngslat = jj;
    return 0;
} /* ckrack_ */

#undef islate
#undef dnum
#undef anum
#endif
