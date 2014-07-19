*
* $Id: mxmlrt.s,v 1.1.1.1 1996/02/15 17:54:04 mclareni Exp $
*
* $Log: mxmlrt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:04  mclareni
* Kernlib
*
*
. **************************************
.
.     SUBROUTINE MXMLRT( A, B, X, NI, NJ )       *** F 110 ***
.     DIMENSION A(NJ,NI), B(NJ,NJ), X(NI,NI)
.     X = A * B * AT                   . AT = A TRANSPOSED
.     SUBROUTINE MXMLRT( A, B, X, NI, NJ )
.     DIMENSION A(NI,NJ), B(NJ,NJ), X(NI,NI)
.     X = AT * B * A
. *   ERROR EXIT IF NI.LE.0 .OR. NJ.LE.0
.     MATRICES STORED USING CERN CONVENTION: AIJ STORED IN A(J,I)
.
. **************************************
 
          AXR$
          FORT$
HOLDJM1   EQU       A15+1
HOLDIM1   EQU       A15+2
 
SLATE     INFO  2  2
$(2),SAVA6  RES     2
 
$(1),FIRS01
          +         'MXMLTR'
MXMLTR*
          S         X11,A15+1
          J         MXML01
          +         'MXMLRT'
MXMLRT*
          SZ        A15+1
MXML01
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,3,XARG         . LOC(NI), LOC(NJ)
          DS        A6,SAVA6
          L         R2,0,A2           . NI
          L         A2,2,XARG         . LOC(X)
          L         R3,0,A3           . NJ
          DL        A0,0,XARG         . LOC(A), LOC(B)
          DL        A4,R2             . NI, NJ
          JGD       R2,$+2            . NI-1
          J         LMJER1            . ERROR EXIT IF NI.LE.0
          JGD       R3,$+2            . NJ-1
LMJER1    SLJ       GERR$             . ERROR EXIT IF NJ.LE.0
          L         A6,A4
          LXI,U     A1,1
          L,U       A3,A6
          MI        A6,A6
          S,H2      A0,MXML69
          LXI,U     A2,1
          S,H2      A1,MXML67
          S         A7,R1
          BT        A2,0,*A3          . VZERO( X, NI**2 )
          S,H2      A7,MXML41
          LXI       A2,A4             . NI, LOC(X(...))
          L         A3,A0
          TZ        A15+1
          J         MXML30
          MSI       A4,A5             . NI*NJ
          LXI,U     A0,1              . MXMLRT
          S,H2      A5,MXML66         . SET PARAMS FOR  MXMLRT
          AN,U      A4,1              . NI*NJ - 1
          LXI       A3,A5             . J,LOC(A)
          S,H2      A4,MXML63
          J         MXML36
MXML30                                . SET PARAMS FOR  MXMLTR
          L,U       A6,1
          MSI       A5,A4             . NI*NJ
          SZ,H2     MXML63
          LXI       A0,A4             . NI,LOC(A)
          S,H2      A6,MXML66
          LXI,U     A3,1
MXML36
          S,H2      A5,MXML60
          S         R3,HOLDJM1
          S         R2,HOLDIM1
MXML40
MXML41    AN,U      A2,$-$            . -NI**2
MXML42
          L         R1,HOLDJM1
          SZ        A4
MXML46
          L         A6,0,*A1
          FM        A6,0,*A0
          FA        A4,A6
          JGD       R1,MXML46
          L         R1,HOLDIM1
MXML52
          L         A6,0,*A3
          L         A5,0,A2
          FM        A6,A4
          FA        A5,A6
          S         A5,0,*A2
          JGD       R1,MXML52
MXML60    AN,U      A0,$-$            . -NJ  OR  -NI*NJ
MXML63    AN,U      A3,$-$            . -NI*NJ+1  OR  0
          JGD       R3,MXML40
MXML66    A,U       A0,$-$            . +NJ  OR  +1
MXML67    LXM,U     A1,$-$            . LOC(B)
          A,U       A2,1
MXML69    LXM,U     A3,$-$            . LOC(A)
          L         R3,HOLDJM1
          JGD       R2,MXML40
          DL        A6,SAVA6
          J         RETUR5
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS
#include "gerr_c.inc"
          INFO  010  03
$(3),DIAG1 +        0775004,MXMLRT
          +         DYN$
          +         'MXMLRT,TR   '
DIAG11    +         0776102,MXMLTR
          +         DIAG1
          ON    GERMAX
F         FORM      6,6,6,18
DIAG2     +         0776702,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'MXMLRT,TR(,,,NI,NJ) ERROR: NI OR NJ.LE.0'
LDIAG3    EQU       $-DIAG3
          OFF . GERMAX
          ON    GERMAX=0
DIAG2     +         0776202,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF . GERMAX=0
#endif
          END
#ifdef CERNLIB_TCGEN_MXMLRT
#undef CERNLIB_TCGEN_MXMLRT
#endif
