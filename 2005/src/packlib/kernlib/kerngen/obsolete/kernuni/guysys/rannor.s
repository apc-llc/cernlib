*
* $Id: rannor.s,v 1.1.1.1 1996/02/15 17:54:08 mclareni Exp $
*
* $Log: rannor.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:08  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     SUBROUTINE RANNOR(A,B)                     *** V 100 ***
.     GENERATES TWO INDEPENDENT SET OF VALUES WITH NORMAL DISTRIBUTIONS(0,1)
.     METHOD: X AND Y ARE RANDOM NUMBERS UNIFORMLY DISTRIBUTED WITHIN 0,1
.     A = SIN(2.*PI*X)*SQRT(-2.*ALOG(Y))
.     B = COS(2.*PI*X)*SQRT(-2.*ALOG(Y))
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
SLATE     INFO 2 2
$(2)
COSI
ARGCOS    RES       1
ARGSQRT
ARGLOG    RES       1
SINI      RES       1
 DO FTN ,SAVARG     RES  2
 
$(0)
W         +5                   . AMORCE
W1        +131071              . AMORCE FIXE POUR L'AMPLITUDE
TWO       +2.0
T        + 1220703125          . FACTEUR CONSTANT, 5**PREMIER
DEUPI    + 6.2831853072        . 2*PI
          ON    FORE
RANA      +         'RANNOR'
DYN$      +         0
          OFF
          ON    FTN
LOCARG2
LOCARG1   +         ARGCOS
LOCARG4
LOCARG3   +         ARGLOG
          OFF . FTN
 
$(1),FIRS01
RANNOR*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
 DO FTN , DL        A4,0,XARG          . LOC(A), LOC(B)
          L         A0,W               . FACTEUR VARIABLE
          S,H2      X11,DYN$
         MSI     A0,T          . FACTEUR CONSTANT
 DO FTN , DS        A4,SAVARG
          L,U       A2,0170            . EXPOSANT POUR  X.LT.1
         AND     A0,(0377777777777)    . ENLEVER LE SIGNE
         LA      A4,W1         . 2-EME FACTEUR VARIABLE
         SA      A1,W
         MSI     A4,T          . FACTEUR CONSTANT
         LCF     A2,A1
         AND     A4,(0377777777777)    . ENLEVER LE SIGNE
         FM      A3,DEUPI
         SA      A5,W1
         SA      A3,ARGCOS
          LCF       A2,A5
 DO FTN , L,U       A0,LOCARG1
          S         A3,ARGLOG
          ON    FORE++FORT**OPTR
LMJSIN    LMJ       X11,SIN
          +         ARGCOS
 DO FORE , +        $-RANNOR-2,RANA
 DO NNOP , NOP
          OFF . FORE++FORT**OPTR
          ON    FORT**(-OPTR)
          L,U       A0,ARGCOS
          LXI,U     X11,RMATH$
LMJSIN    LIJ       X11,SIN$
          OFF . FORT**(-OPTR)
          ON    FTN
LMJSIN    LMJ       X11,SIN
          OFF . FTN
          S         A0,SINI
          ON    FORE++FORT**OPTR
LMJCOS    LMJ       X11,COS
          +         ARGCOS
 DO FORE , +        $-RANNOR-2,RANA
 DO NNOP , NOP
          OFF . FORE++FORT**OPTR
          ON    FORT**(-OPTR)
          L,U       A0,ARGCOS
          LXI,U     X11,RMATH$
LMJCOS    LIJ       X11,COS$
          OFF . FORT**(-OPTR)
          ON    FTN
          L,U       A0,LOCARG2
LMJCOS    LMJ       X11,COS
          OFF . FTN
         SA      A0,COSI       .  COS(2*PI*N1)
          ON    FORE++FORT**OPTR
LMJLOG    LMJ       X11,ALOG
          +         ARGLOG
 DO FORE , +        $-RANNOR-2,RANA
 DO NNOP , NOP
          OFF . FORE++FORT**OPTR
          ON    FORT**(-OPTR)
          L,U       A0,ARGLOG
          LXI,U     X11,RMATH$
LMJLOG    LIJ       X11,ALOG$
          OFF . FORT**(-OPTR)
          ON    FTN
          L,U       A0,LOCARG3
LMJLOG    LMJ       X11,ALOG
          OFF . FTN
          FM        A0,TWO
         SNA     A0,ARGSQRT    .  - ALOG(N2)
          ON    FORE++FORT**OPTR
LMJSQR    LMJ       X11,SQRT
          +         ARGSQRT
 DO FORE , +        $-RANNOR-2,RANA
 DO NNOP , NOP
          OFF . FORE++FORT**OPTR
          ON    FORT**(-OPTR)
          L,U       A0,ARGSQRT
          LXI,U     X11,RMATH$
LMJSQR    LIJ       X11,SQRT$
          OFF . FORT**(-OPTR)
          ON    FTN
          L,U       A0,LOCARG4
LMJSQR    LMJ       X11,SQRT
          OFF . FTN
          L         A2,A0
          L         A4,A0
          L         X11,DYN$
          FM        A2,SINI
 DO FOR , DL        A0,0,XARG
 DO FTN , DL        A0,SAVARG
          FM        A4,COSI
          S         A2,0,A0            . A
          S         A4,0,A1            . B
          J         RETUR2
 
.     DIAGNOSTIC PACKETS FOR FORT
          ON    FORT++FTN
#include "gerr_c.inc"
          RES       1
          OFF .
          ON    FORT
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,RANNOR
          +         0,DYN$
          +         'RANNOR'
 DO OPTR ,DIAG2  +  0776202,LMJSIN+1
 DO OPTR=0 ,DIAG2 + 0777302,LMJSIN+1
          +         LMJSIN-FIRS01,DIAG1
 DO OPTR ,DIAG3  +  0776202,LMJCOS+1
 DO OPTR=0 ,DIAG3 + 0777302,LMJCOS+1
          +         LMJCOS-FIRS01,DIAG1
 DO OPTR ,DIAG4  +  0776202,LMJLOG+1
 DO OPTR=0 ,DIAG4 + 0777302,LMJLOG+1
          +         LMJLOG-FIRS01,DIAG1
 DO OPTR ,DIAG5  +  0776202,LMJSQR+1
 DO OPTR=0 ,DIAG5 + 0777302,LMJSQR+1
          +         LMJSQR-FIRS01,DIAG1
          OFF
         END
