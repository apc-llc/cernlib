*
* $Id: vunit.s,v 1.1.1.1 1996/02/15 17:54:07 mclareni Exp $
*
* $Log: vunit.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:07  mclareni
* Kernlib
*
*
. *************************************
.     SUBROUTINE VUNIT( A, X, N )                *** F 121 ***
.     X(I) = A(I) / VMOD(A)
.     USES JZ TO TEST VMOD(A,N)=0  INSTEAD OF  TOP  A0,(0400400,0)
.      BECAUSE THE RESULT OF VMOD IS NORMALISED
.     DO NOTHING IF N.EQ.0
. *   N.LT.0 DETECTED AND SIGNALED BY VMOD
.
. *************************************
 
          AXR$
          FORT$
 
SLATE     INFO      2 2
$(2),VUNB RES       1                  . ALSO USED BY UMOD
          ON    FTN
VUN1
VUN2      RES       4
          OFF . FTN
          ON   FORE
$(0)
VUNA      + 'VUNIT '              . WALK BACK PACKET
SAVX11    +         0
          OFF
          ON    FORT++FTN
#include "gerr_c.inc"
          RES       1
SAVX11    RES       1
          OFF
$(1),FIRS01
VUNERR  . (CALL GERR$ MUST NOT BE THE LAST INSTR. OF A ROUTINE)
          L         X11,SAVX11
LMJER1    SLJ       GERR$              . ERROR EXIT IF VMOD(A,N)=0.
 
          +         'VUNIT '
VUNIT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,1,XARG          . LOC(X), LOC(N)
          S,H2      X11,SAVX11
          DS        A2,VUN2+2
          L         A2,0,XARG          . LOC(A)
 DO FTN , L,U       A0,VUN1
          DS        A2,VUN1
          S         A2,VUN2
LMJVMD    LMJ       X11,VMOD           . CALL VMOD(A,N)
 DO FOR ,VUN1  RES  2
 DO FORE , +        $-FIRS01-3,VUNA
 DO NNOP , NOP
          JN        A0,VUN4            . ERROR N<0  DIAGNOSED BY VMOD
          JZ        A0,VUNERR
          L         A2,(1.0)
          FD        A2,A0         . 1. / VMOD(A)
          S         A2,VUNB
          ON    FTN
          L,U       A1,VUNB
          L,U       A0,VUN2
          S         A1,VUN2+1
          OFF . FTN
LMJVSC    LMJ       X11,VSCALE
          ON    FOR
VUN2      RES 1                   . LOC A
          +         VUNB
          RES 2                   . LOC(X) AND LOC(N)
          OFF . FOR
 DO FORE , +        $-FIRS01-5,VUNA
 DO NNOP , NOP
VUN4
          L         X11,SAVX11
          J         RETUR3
 
.     DIAGNOSTIC PACKETS FOR FORT
          ON    FORT
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,VUNIT
          +         0,DYN$+1
          +         'VUNIT '
DIAG4     F         077,062,2,LMJVMD+1
          +         LMJVMD-FIRS01,DIAG1
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$+1
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VUNIT( A,X,N ) ERROR: VMOD(A,N)=0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
DIAG5     F         077,062,2,LMJVSC+1
          +         LMJVSC-FIRS01,DIAG1
          OFF
          END
#ifdef CERNLIB_TCGEN_VUNIT
#undef CERNLIB_TCGEN_VUNIT
#endif
