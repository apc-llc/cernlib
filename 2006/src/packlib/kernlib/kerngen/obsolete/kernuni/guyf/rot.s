*
* $Id: rot.s,v 1.1.1.1 1996/02/15 17:54:05 mclareni Exp $
*
* $Log: rot.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:05  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.      SUBROUTINE ROT( A, TH, B )                *** F 118 ***
.     DIMENSION A(3),B(3)
.     ROTATE VECTOR A BY TH RADIANS AROUND THE Z AXIS TO GIVE B
.     B(1) = A(1)*COS(TH) - A(2)*SIN(TH)
.     B(2) = A(1)*SIN(TH) + A(2)*COS(TH)
.     B(3) = A(3)
. *   SINGLE PRECISION A,TH,B
. *   ANY OVERLAP
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
SLATE     INFO      2 2
$(2)
ROTB      RES 1
ROTC      RES 1
LCARGB    RES       1                 . LCARGB BEFORE LCARGA
LCARGA    RES       1
 
.     WALK BACK PACKET FOR 11A
          ON    FORE
$(0)
ROTA      'ROTA'
DYN$      +         0
          OFF
          ON    FTN
$(0)
LOCARG    +         ROTB
          OFF . FTN
 
$(1),FIRS01
ROT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(A), LOC(TH)
          L         A1,2,XARG
          S,H2      X11,DYN$
          L         A3,0,A3            . TH
          DS        A1,LCARGB
 DO FTN , L,U       A0,LOCARG
          S         A3,ROTB
          ON    FORE++FORT**OPTR
LMJSI1    LMJ       X11,SIN
          +         ROTB
 DO FORE , +        $-ROT-2,ROTA
 DO NNOP , NOP
          OFF . FORE++FORT**OPTR
          ON    FORT**(-OPTR)
          L,U       A0,ROTB
          LXI,U     X11,RMATH$
LMJSI1    LIJ       X11,SIN$
          OFF . FORT**(-OPTR)
          ON    FTN
LMJSI1    LMJ       X11,SIN
          OFF . FTN
          SA        A0,ROTC .     ROTC=SIN(TH)
          ON    FORE++FORT**OPTR
LMJCO1    LMJ       X11,COS
          +         ROTB
 DO FORE , +        $-ROT-2,ROTA
 DO NNOP , NOP
          OFF . FORE++FORT**OPTR
          ON    FORT**(-OPTR)
          L,U       A0,ROTB
          LXI,U     X11,RMATH$
LMJCO1    LIJ       X11,COS$
          OFF . FORT**(-OPTR)
          ON    FTN
          L,U       A0,LOCARG
LMJCO1    LMJ       X11,COS
          OFF . FTN
          L,H2      X11,DYN$
          LA        A4,ROTC .
          L         A2,LCARGA
          LA        A3,A0 .       A0=A3=COS(TH)
          FM        A0,1,A2 .     A(2)*COS(TH)
          FM        A4,0,A2 .     A(1)*SIN(TH)
          FA        A0,A4 .       A0=A(2)*COS(TH)+A(1)*SIN(TH)=B(2)
          FM        A3,0,A2 .     A(1)*COS(TH)
          LA        A1,2,A2 .    A1=A(3)=B(3)
          LA        A4,1,A2 .
          FM        A4,ROTC .     A(2)*SIN(TH)
          L         A2,LCARGB
          FAN       A3,A4 .
          DS        A0,1,A2 .
          SA        A3,0,A2 .
          J         RETUR3
 
.     DIAGNOSTIC PACKETS FOR FORT
          ON    FORT++FTN
#include "gerr_c.inc"
          RES  1
          OFF
          ON    FORT
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,ROT
          +         0,DYN$
          +         'ROT   '
 DO OPTR ,DIAG2  +  0776202,LMJSI1+1
 DO OPTR=0 ,DIAG2 + 0777302,LMJSI1+1
          +         LMJSI1-FIRS01,DIAG1
 DO OPTR ,DIAG3  +  0776202,LMJCO1+1
 DO OPTR=0 ,DIAG3 + 0777302,LMJCO1+1
          +         LMJCO1-FIRS01,DIAG1
          OFF
      END
#ifdef CERNLIB_TCGEN_ROT
#undef CERNLIB_TCGEN_ROT
#endif
