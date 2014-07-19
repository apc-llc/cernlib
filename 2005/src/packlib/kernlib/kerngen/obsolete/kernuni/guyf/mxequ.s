*
* $Id: mxequ.s,v 1.1.1.1 1996/02/15 17:54:04 mclareni Exp $
*
* $Log: mxequ.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:04  mclareni
* Kernlib
*
*
. **************************************
.
.     SUBROUTINE MXEQU( A, B, NI, NJ )           *** F 109 ***
.     DIMENSION A(NI,NI),  B(NJ,NI)
.     SOLVES MATRIX EQUATION  AX = B    FOR X
.     MXEQU JUST TRANSFORMS  MATRIX  A   IF  NJ=0
.     SUBROUTINE MXEQU1( A, B, NI, NJ )
.     SAME AS MXEQU, BUT ASSUMES MATRIX  A  TRANSFORMED BY A PREVIOUS CALL
.     TO MXEQU
.     DO NOTHING IF  NI = 0
. *   ERROR EXIT IF  NI.LT.0 .OR. NJ.LT.0
.     METHOD: MILNE, NUMERICAL CALCULUS
.
. **************************************
 
          AXR$
          FORT$
XA2       EQU       X10
XA5       EQU       X11
XA1       EQU       A0
XA3       EQU       A1
XA6       EQU       A2
XA4       EQU       A3
AL1       EQU       A4
AL2       EQU       A5
AC1       EQU       A9
AC2       EQU       A10
AUX1      EQU       AC1
AUX2      EQU       AC2
INCXA4    EQU       A12
INCXA5    EQU       A14
INCXA2    EQU       A15
HOLDIM1   EQU       A8
HOLDAII   EQU       A7
HOLDNI    EQU       A15+2
HOLDNIM1  EQU       A15+1
RLIG      EQU       R3
RNMI      EQU       R2
RELT      EQU       R1
 
XB3       EQU       A1
XB2       EQU       A2
XC2       EQU       A1
XC3       EQU       A2
HOLDIIM1  EQU       A3
ANIMI     EQU       A3
AB1       EQU       A4
AB2       EQU       A5
AUX3      EQU       AB1
INCXB2    EQU       A7
INCXC2    EQU       A7
HOLDNJ    EQU       A14
HOLDNJM1  EQU       A15
RNJ       EQU       R2
 
SLATE     INFO  2  2
$(2)
SAVX10    RES  1
SAVX11    RES  1
SAVA6     RES  2
SAVA8     RES  2
SAVA10    RES  2
SAVA12    RES  1
SAVA14    RES  2
SAVLOCA   RES  1
SAVBJ     RES  2
 
$(1),FIRS01
          +         'MXEQU1'
MXEQU1*
          L,U       A5,1
          J         MXEQ01
          +         'MXEQU '
MXEQU*
          L,U       A5,0
MXEQ01
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,2,XARG         . LOC(NI), LOC(NJ)
          DL        A0,0,XARG         . LOC(A), LOC(B)
          L         A4,0,A2           . NI
          L         A2,0,A3           . NJ
          S         A4,HOLDNI
          JZ        A4,RETUR4         . DO NOTHING IF NI=0
          JGD       A4,$+2
LMJER1    SLJ       GERR$
          TZ        A2
          JN        A2,LMJER1         . ERROR EXIT  IF  NJ.LT.0
          DS        A1,SAVBJ
          DS        A6,SAVA6
          LXI,U     XA1,1
          DS        A14,SAVA14
          S         A4,HOLDNIM1
          JNZ       A5,MXQ101
          S         X11,SAVX11
          S         X10,SAVX10
          S         A0,SAVLOCA
          DS        A8,SAVA8
          L         XA5,XA1
          DS        A10,SAVA10
          S         A4,RLIG
          S         A12,SAVA12
          L,U       HOLDIM1,0         . I-1
          LXI       XA5,HOLDNI
          J         MXEQ70
.
. TRANSFORM MATRIX  A
.
MXEQ40
          L         RNMI,RLIG
MXEQ42
          ANU,U     HOLDIM1,1         .  I - 2
          L         AL1,0,XA3
          S         HOLDIM1+1,RELT
          L         AC1,0,XA6
          JZ        HOLDIM1,MXEQ50
MXEQ44
          L         AL2,0,*XA1
          L         AC2,0,*XA4
          FM        AL2,0,*XA2
          FM        AC2,0,*XA5
          FAN       AL1,AL2
          FAN       AC1,AC2
          JGD       RELT,MXEQ44
MXEQ50
          FD        AL1,HOLDAII
          L         AC2,0,*XA4
          S         AL1,0,*XA3
          A         XA4,INCXA4
          FM        AC2,0,XA5
          AN        XA1,HOLDIM1
          FAN       AC1,AC2
          A         XA2,INCXA2
          S         AC1,0,*XA6
          A         XA5,INCXA5
          JGD       RNMI,MXEQ42
          A,U       HOLDIM1,1         . (I-1) = (I-1) + 1
          A         XA1,HOLDNI        . 1. +NI
MXEQ70
          AU        XA1,HOLDIM1       . 1. LOC(A(I,I))
          DLN       AUX1,HOLDNIM1     . 2.
          L         HOLDAII,0,XA1+1   . 1. A(I,I)
          A,U       XA5,1
          A         AUX1,HOLDIM1      . 2.  -(NI-I)
          A,U       XA3,1             . 1.
          MSI       AUX2,HOLDIM1       . 2. -NI*(I-1)
          AU        XA3,HOLDNI        . 1.
          SN        AUX1,INCXA4       . 2.
          ANU       XA6,HOLDIM1       . 1. TO GET XA4
          AU,U      AUX2,1            . -NI*(I-1) + 1
          AN,U      XA4,1             . 1.
          DS        AUX2,INCXA5
          LXI       XA6,HOLDNI
          S         XA5,XA2
          JGD       RLIG,MXEQ40
 
          L         X10,SAVX10
          L         X11,SAVX11
          L         A0,SAVLOCA
          DL        A1,SAVBJ
          DL        A8,SAVA8
          DL        A10,SAVA10
          L         A12,SAVA12
.
.     TRANSFORM MATRIX  B
.
MXQ101
          JZ        A2,MXQFIN
          ANU,U     A2,1              . NJ - 1
          DS        A2,HOLDNJ
          L         RLIG,HOLDNI
          L         XB2,XB3
          L,U       HOLDIIM1,0        .  I-1
          LXI       XB2,HOLDNJ
          LXI,U     XB3,1
          J         MXQ170
MXQ140
          L         RNJ,HOLDNJM1
MXQ142
          ANU,U     HOLDIIM1,1
          S         HOLDIIM1+1,RELT
          L         AB1,0,XB3
          JZ        HOLDIIM1,MXQ150
MXQ144
          L         AB2,0,*XA1
          FM        AB2,0,*XB2
          FAN       AB1,AB2
          JGD       RELT,MXQ144
MXQ150
          FD        AB1,0,XA1
          AN        XB2,INCXB2
          S         AB1,0,*XB3
          AN        XA1,HOLDIIM1
          JGD       RNJ,MXQ142
          A,U       HOLDIIM1,1        .  (I-1) = (I-1) + 1
          A         XA1,HOLDNI
          LXM       XB2,SAVBJ
MXQ170
          L         AUX3,HOLDNJ       . NJ
          MSI       AUX3,HOLDIIM1     . NJ*(I-1)
          AN,U      AUX3,1            . NJ*(I-1) - 1
          S         AUX3,INCXB2
          JGD       RLIG,MXQ140
.
.     GET SOLUTION  X  (STORED INTO B )
.
          AN,U      XB3,1             . BECOMES XC2
          L,U       ANIMI,1           . NI - I
          ANU       XC2,HOLDNJ        . BECOMES XC3
          L         RLIG,HOLDNIM1
          LXI       XC2,HOLDNJ
          LXI,U     XC3,-1
          J         MXQ270
MXQ240
          L         RNJ,HOLDNJM1
MXQ242
          ANU,U     ANIMI,1
          S         ANIMI+1,RELT
          L         AB1,0,XC3
MXQ244
          L         AB2,0,*XA1
          FM        AB2,0,*XC2
          FAN       AB1,AB2
          JGD       RELT,MXQ244
          S         AB1,0,*XC3
          AN        XA1,ANIMI
          AN        XC2,INCXC2
          JGD       RNJ,MXQ242
          A,U       ANIMI,1           . (NI-I) = (NI-I) + 1
MXQ270
          L         AUX3,HOLDNJ
          AN        XA1,HOLDNI
          MSI       AUX3,ANIMI        . NJ*(NI-I)
          AN,U      XA1,1
          A,U       AUX3,1            . NJ*(NI-I) + 1
          S         AUX3,INCXC2
          JGD       RLIG,MXQ240
MXQFIN
          DL        A14,SAVA14
          DL        A6,SAVA6
          J         RETUR4
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO  010  03
$(3),DIAG1 F        077,050,LDIAG1,MXEQU
          +         DYN$
          +         'MXEQU,MXEQU1'
LDIAG1    EQU       $-DIAG1
DIAG11    +         0776102,MXEQU1
          +         DIAG1
          ON    GERMAX
DIAG2     +         0776702,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'MXEQU,EQU1(,,NI,NJ) ERROR: NI<0 OR NJ<0'
LDIAG3    EQU       $-DIAG3
          OFF . GERMAX
          ON    GERMAX=0
DIAG2     +         0776202,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF . GERMAX=0
#endif
          END
#ifdef CERNLIB_TCGEN_MXEQU
#undef CERNLIB_TCGEN_MXEQU
#endif
