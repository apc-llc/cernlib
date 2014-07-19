*
* $Id: vdotn.s,v 1.1.1.1 1996/02/15 17:54:06 mclareni Exp $
*
* $Log: vdotn.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:06  mclareni
* Kernlib
*
*
. *************************************
.     FUNCTION VDOTN( A, B, N )                  *** F 121 ***
.     'COSINE' OF THE N-VECTORS A AND B
.     VDOTN = VDOT(A,B,N) / VMOD(A,N)*VMOD(B,N)
.     RETURNS VDOTN=0. IF VMOD(A,N)*VMOD(B,N)=0
. *   ERROR EXIT IF  N.LE.0   (==>VDOTN=0.)
.
. *************************************
 
          AXR$
          FORT$
 
          ON    FORE
$(0)
VDTA        'VDOTN '                   . WALK-BACK PACKET
SAVX11    +         0
          OFF . FORE
          ON    FORT++FTN
#include "gerr_c.inc"
SAVX11    RES       1
          OFF . FORT++FTN
 
SLATE     INFO      2 2
$(2),VDTB RES       1
 DO FTN , RES       2
 
          ON    FTN
$(0),LOCARG  +      VDTB
          OFF . FTN
$(1),FIRS01
          +         'VDOTN '
VDOTN*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A1,2,XARG          . LOC(N)
          DL        A2,0,XARG          . LOC(A), LOC(B)
 DO FOR , SZ        A0                . (FTN: H1(A0)=0)
          L         R1,0,A1            . N
          S,H2      X11,SAVX11
          JGD       R1,$+2             . N-1
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LE.0
 DO FTN , DS        A2,VDTB+1
          S         R1,R2
          LMJ       X11,VMOD1     . A**2
          S         A0,R3              .
          L         R1,R2
          L         A2,A3
          LMJ       X11,VMOD1     . B**2
          FM        A0,R3              . A**2 * B**2
          S         A0,VDTB
          ON    FORE++FORT**OPTR
LMJSQ1    LMJ       X11,SQRT
          +         VDTB
 DO FORE , +        $-FIRS01-2,VDTA
 DO NNOP , NOP
          OFF . FORE++FORT**OPTR
          ON    FORT**(-OPTR)
          L,U       A0,VDTB
          LXI,U     X11,RMATH$
LMJSQ1    LIJ       X11,SQRT$
          OFF . FORT**(-OPTR)
          ON    FTN
          L,U       A0,LOCARG
LMJSQ1    LMJ       X11,SQRT
          OFF . FTN
          L         X11,SAVX11
          JZ        A0,RETUR3          . (THE RESULT OF SQRT IS NORMALIZED)
 DO FOR , DL        A2,0,XARG          . LOC(A), LOC(B)
 DO FTN , DL        A2,VDTB+1
          S         A0,R3
          SZ        A0
          LXI,U     A2,1
          LXI,U     A3,1
VDT2      L         A4,0,*A2           . A(I)*B(I)
          FM        A4,0,*A3
          FA        A0,A4
          JGD       R2,VDT2
          FD        A0,R3
          J         RETUR3
/
. $$$*****$$$*****$$$*****$$$*****
.     FUNCTION VMOD(A,N)                         *** F 121 ***
.     VMOD = SQRT( A(1)**2+...+A(N)**2 )
.     RETURNS VMOD=0. IF N.EQ.0
. *   ERROR EXIT GERRB$ IF N.LT.0
. $$$*****$$$*****$$$*****$$$*****
.
 
          ON    FORE
$(0)
VMDA      'VMOD'                       . WALK BACK PACKET
          +0
          OFF
$(1)
          +         'VMOD  '
VMOD*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(A), LOC(N)
 DO FORE , S,H2     X11,VMDA+1
 DO FORT++FTN , S,H2  X11,SAVX11
          L         A0,0,A3            . N
          JZ        A0,RETUR2          . RETURNS VMOD=0. IF N=0
          JGD       A0,$+2             . N-1
LMJER2    SLJ       GERR$              . ERROR EXIT IF N.LT.0
          S         A0,R1
          LMJ       X11,VMOD1     . A**2
          S         A0,VDTB
          ON    FORE++FORT**OPTR
LMJSQ2    LMJ       X11,SQRT
          +         VDTB
 DO FORE , +        $-FIRS01-2,VMDA
 DO NNOP , NOP
          OFF . FORE++FORT**OPTR
          ON    FORT**(-OPTR)
          L,U       A0,VDTB
          LXI,U     X11,RMATH$
LMJSQ2    LIJ       X11,SQRT$
          OFF . FORT**(-OPTR)
          ON    FTN
          L,U       A0,LOCARG
LMJSQ2    LMJ       X11,SQRT
          OFF . FTN
 DO FORE , L        X11,VMDA+1
 DO FORT++FTN , L   X11,SAVX11
          J         RETUR2
 
VMOD1
          LXI,U     A2,1
          SZ        A0
VMOD2     LMA       A4,0,*A2
          FM        A4,A4
          FA        A0,A4
          JGD       R1,VMOD2
          J         0,X11
 
.     DIAGNOSTIC PACKETS FOR FORT
          ON    FORT
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,VDOTN
          +         0,DYN$
          +         'VDOTN '
DIAG11    F         077,050,3,VMOD
          +         0,DYN$
          +         'VMOD  '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VDOTN( A,B,N ) ERROR: N.LE.0'
LDIAG3    EQU       $-DIAG3
DIAG12    F         077,067,2,0
          +         LMJER2-FIRS01,DYN$
DIAG13    F         077,047,LDIAG13,LMJER2+1
          +         LDIAG13-2,DIAG12
 'VMOD( A,N ) ERROR: N<0'
LDIAG13   EQU       $-DIAG13
          ON    OPTR
DIAG14    F         077,067,2,LMJSQ1
          +         LMJSQ1-FIRS01,DYN$
DIAG24    F         077,067,2,LMJSQ2
          +         LMJSQ2-FIRS01,DYN$
          OFF . OPTR
          ON    OPTR=0
DIAG14    +         0777302,LMJSQ1+1
          +         LMJSQ1-FIRS01,DIAG1
DIAG24    +         0777302,LMJSQ2+1
          +         LMJSQ2-FIRS01,DIAG11
          OFF . OPTR=0
          OFF
          ON        GERMAX=0
          ON    DIAGP
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
DIAG12    F         077,062,2,LMJER2+1
          +         LMJER2-FIRS01,DIAG11
          OFF . DIAGP
 DO OPTR ,DIAG14  +  0776202,LMJSQ1+1
 DO OPTR=0 ,DIAG14 + 0777302,LMJSQ1+1
          +         LMJSQ1-FIRS01,DIAG1
 DO OPTR ,DIAG24  +  0776202,LMJSQ2+1
 DO OPTR=0 ,DIAG24 + 0777302,LMJSQ2+1
          +         LMJSQ2-FIRS01,DIAG11
          OFF
          OFF
          END
#ifdef CERNLIB_TCGEN_VDOTN
#undef CERNLIB_TCGEN_VDOTN
#endif
#ifdef CERNLIB_TCGEN_VMOD
#undef CERNLIB_TCGEN_VMOD
#endif
