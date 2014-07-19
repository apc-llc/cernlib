*
* $Id: mxmpy.s,v 1.1.1.1 1996/02/15 17:54:05 mclareni Exp $
*
* $Log: mxmpy.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:05  mclareni
* Kernlib
*
*
. **************************************
.
.     SUBROUTINE MXMPY ( A, B, X, NI, NJ, NK )   *** F 110 ***
.     DIMENSION A(NJ,NI), B(NK,NJ), X(NK,NI)
.     X = A*B
.     IF  NJ=0   X IS FILLED WITH ZEROS
.     SUBROUTINE MXMAD ( A, B, X, NI, NJ, NK )
.     X = A*B + X
.     IF  NJ=0   X IS NOT MODIFIED
.     SUBROUTINE MXMUB ( A, B, X, NI, NJ, NK )
.     X = A*B - X
.     IF  NJ=0   X IS REPLACED BY -X
. *   ERROR EXIT IF NI.LE.0 .OR. NJ.LT.0 .OR. NK.LE.0
.
.     SUBROUTINE MXMPY1( A, Q, X, NI, NJ, NK )   *** F 110 ***
.     DIMENSION A(NJ,NI), Q(NJ,NK), X(NK,NI)
.     X = A*QT
.     SUBROUTINE MXMAD1( A, Q, X, NI, NJ, NK )
.     X = A*QT + X
.     SUBROUTINE MXMUB1( A, Q, X, NI, NJ, NK )
.     X = A*QT - X
.
.     SUBROUTINE MXMPY2( P, B, X, NI, NJ, NK )   *** F 110 ***
.     DIMENSION P(NI,NJ), B(NK,NJ), X(NK,NI)
.     X = PT*B
.     SUBROUTINE MXMAD2( A, Q, X, NI, NJ, NK )
.     X = PT*B + X
.     SUBROUTINE MXMUB2( A, Q, X, NI, NJ, NK )
.     X = PT*B - X
.
.     SUBROUTINE MXMPY3( P, Q, X, NI, NJ, NK )   *** F 110 ***
.     DIMENSION P(NI,NJ), Q(NJ,NK), X(NI,NK)
.     X = PT*QT
.     SUBROUTINE MXMAD3( P, Q, X, NI, NJ, NK )
.     X = PT*QT + X
.     SUBROUTINE MXMUB3( P, Q, X, NI, NJ, NK )
.     X = PT*QT - X
.
. **************************************
 
          AXR$
          FORT$
HOLDKM1   EQU       A15+2
HOLDJ     EQU       A15+1
 
$(0)
ADDX      LA        A3,0,A2            . X(K,I)
SUBX      LNA       A3,0,A2            . -X(K,I)
NOX       SZ        A3
 
$(1),FIRS01
          +         'MXMAD3'
MXMAD3*   L         A4,ADDX
          LN,XU     A5,2
          J         MXMP01
          +         'MXMUB3'
MXMUB3*   L         A4,SUBX
          LN,XU     A5,2
          J         MXMP01
          +         'MXMPY3'
MXMPY3*   L         A4,NOX
          LN,XU     A5,2
          J         MXMP01
 
          +         'MXMAD2'
MXMAD2*   L         A4,ADDX
          LN,XU     A5,1
          J         MXMP01
          +         'MXMUB2'
MXMUB2*   L         A4,SUBX
          LN,XU     A5,1
          J         MXMP01
          +         'MXMPY2'
MXMPY2*   L         A4,NOX
          LN,XU     A5,1
          J         MXMP01
 
          +         'MXMAD1'
MXMAD1*   L         A4,ADDX
          L,U       A5,1
          J         MXMP01
          +         'MXMUB1'
MXMUB1*   L         A4,SUBX
          L,U       A5,1
          J         MXMP01
          +         'MXMPY1'
MXMPY1*   L         A4,NOX
          L,U       A5,1
          J         MXMP01
 
          +         'MXMAD '
MXMAD*    L         A4,ADDX
          J         MXMP00
          +         'MXMUB '
MXMUB*    L         A4,SUBX
          J         MXMP00
          +         'MXMPY '
MXMPY*    L         A4,NOX
MXMP00    L,U       A5,0
MXMP01
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A1,4,XARG          . LOC(NJ), LOC(NK)
          S         A4,MXMP43
          L         R2,0,A2            . NK
          DL        A2,2,XARG          . LOC(X), LOC(NI)
          L         R1,0,A1            . NJ
          DL        A0,0,XARG          . LOC(A), LOC(B)
          LXI,U     A2,1
          L         R3,0,A3            . NI
          S,H2      A1,INCAA1
          DL        A3,R1             . NJ, NK
          JB        A5,MXMP20
          MSI       A4,A3             . NJ*NK
          LXI       A1,R2             . NK
          AN,U      A4,1              . NJ*NK-1
          JN        A5,MXMP22
MXMP18    L         A5,A3             . NJ
          LXI,U     A0,1
          J         MXMP24
MXMP20    LXI,U     A1,1
          L,U       A4,0
          JP        A5,MXMP18
MXMP22    MSI       A3,R3             . NI*NJ
          LXI       A0,R3             . NI
          L,U       A5,1
MXMP24    SM,H2     A4,INCA1
          SM,H2     A3,INCA0
          SM,H2     A5,INCAA0
          JGD       R3,$+2             . NI-1
          J         LMJER1
          JGD       R2,$+2             . NK-1
          J         LMJER1
          JZ        A3,$+3            . (A3=NJ OR NI*NJ)
          TP        R1
LMJER1    SLJ       GERR$
          S         R2,HOLDKM1
          S         R1,HOLDJ
MXMP40                                . LOOP  NI
          L         R2,HOLDKM1        . NK - 1
MXMP42                                . LOOP  NK
MXMP43    +         $-$                . SZ  OR  L  OR  LN  A3
          L         R1,HOLDJ           . NJ-1
          J         MXMP50
MXMP44                                 . LOOP NJ
          L         A4,0,*A1           . B(K,J)
          FM        A4,0,*A0           . A(J,I)
          FA        A3,A4
MXMP50    JGD       R1,MXMP44
          S         A3,0,*A2           . C(K,I)
INCA0     AN,U      A0,$-$
INCA1     AN,U      A1,$-$
          JGD       R2,MXMP42
INCAA0    A,U       A0,$-$
INCAA1    LXM,U     A1,$-$            .  INC, LOC(B)
          JGD       R3,MXMP40
          J         RETUR6
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO  010  03
$(3),DIAG1 F        077,050,LDIAG1,MXMPY
          +         DYN$
          +         'MXMP.PACKAGE'
LDIAG1    EQU       $-DIAG1
DIAG11    +         0775102,MXMPY1
          +         DIAG1
DIAG21    +         0775102,MXMPY2
          +         DIAG1
DIAG31    +         0775102,MXMPY3
          +         DIAG1
DIAG41    +         0775102,MXMAD
          +         DIAG1
DIAG51    +         0775102,MXMAD1
          +         DIAG1
DIAG61    +         0775102,MXMAD2
          +         DIAG1
DIAG71    +         0775102,MXMAD3
          +         DIAG1
DIAG81    +         0775102,MXMUB
          +         DIAG1
DIAG91    +         0775102,MXMUB1
          +         DIAG1
DIAG101   +         0775102,MXMUB2
          +         DIAG1
DIAG111   +         0775102,MXMUB3
          +         DIAG1
          ON    GERMAX
DIAG2     +         0776702,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'MXMP..(,,,NI,NJ,NK) ERROR: NI.LE.0 OR NJ<0 OR NK.LE.0'
LDIAG3    EQU       $-DIAG3
          OFF . GERMAX
          ON    GERMAX=0
DIAG2     +         0776202,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF . GERMAX=0
#endif
          END
#ifdef CERNLIB_TCGEN_MXMPY
#undef CERNLIB_TCGEN_MXMPY
#endif
