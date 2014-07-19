*
* $Id: vdist.s,v 1.1.1.1 1996/02/15 17:54:06 mclareni Exp $
*
* $Log: vdist.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:06  mclareni
* Kernlib
*
*
. **************************************
.
.       FUNCTION  VDIST( X, Y, N )                *** F121 ***
.       FUNCTION VDIST2( X, Y, N )
.     VDIST2 =( X(I)-Y(I) )**2  , I=1,N
.     VDIST = SQRT( VDIST2 )
. *   ERROR EXIT IF  N.LT.0
.
. **************************************
 
          AXR$
          FORT$
 
SLATE     INFO  2  2
$(2),SUM  RES  1
 
          ON    FORE
$(0),WBPKT +        'VDIST '
SAVX11    +         0
          OFF
          ON    FTN
$(0),LOCARG  +      SUM
          OFF . FTN
 
$(1),FIRS01
          +         'VDIST2'
VDIST2*
          L,U       R2,1
          J         ENTRY
          +         'VDIST '
VDIST*
          SZ        R2
ENTRY
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A1,2,XARG          . LOC(N)
          DL        A2,0,XARG          . LOC(X), LOC(Y)
          L         A0,0,A1            . N
          LXI,U     A2,1
          JZ        A0,RETUR3
          JGD       A0,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LT.0
          S         A0,R1
          LXI,U     A3,1
 .        SZ        A0                 . NOT NEEDED
VDI2
          L         A4,0,*A2           . X(I)
          FAN       A4,0,*A3           . X(I) - Y(I)
          FM        A4,A4              . (X(I)-Y(I))**2
          FA        A0,A4              . SUM
          JGD       R1,VDI2
          TZ        R2
          J         RETUR3             . RETURN IF VDIST2
          S,H2      X11,SAVX11
          S         A0,SUM
 DO FTN , L,U       A0,LOCARG
LMJSQR    LMJ       X11,SQRT
 DO FOR , +         SUM
 DO FORE , +        $-FIRS01-2,WBPKT
 DO NNOP , NOP
          L         X11,SAVX11
          J         RETUR3
 
.        DIAGNOSTIC PACKETS FOR FORT
          ON    FORT++FTN
#include "gerr_c.inc"
SAVX11    RES       1
          OFF . FORT++FTN
          ON    FORT
          INFO  010  3
$(3),DIAG1  +       0775003,VDIST
          +         DYN$
          +         'VDIST '
          ON  DIAGP
          ON  GERMAX
DIAG2     F         077,062,2,0
          +         LMJER1-FIRS01,DIAG1
F         FORM      6,6,6,18
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VDIST,VDIST2(X,Y,N) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON  GERMAX=0
DIAG2     +         0776202,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
          OFF  . DIAGP
DIAG4     +         0776202,LMJSQR+1
          +         LMJSQR-FIRS01,DIAG1
          OFF  . FORT
          END
#ifdef CERNLIB_TCGEN_VDIST
#undef CERNLIB_TCGEN_VDIST
#endif
