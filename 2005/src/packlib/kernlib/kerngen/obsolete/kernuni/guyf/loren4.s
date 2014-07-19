*
* $Id: loren4.s,v 1.1.1.1 1996/02/15 17:54:03 mclareni Exp $
*
* $Log: loren4.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:03  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     SUBROUTINE  LOREN4( DIR, PIN, POUT )       *** U 101 ***
.     DIMENSION  DIR(4),  PIN(4),  POUT(4)
.     COMPUTES THE NEW COORDINATES  POUT  OF 4-VECTOR  PIN IN THE
.     LORENTZ TRANSFORMATION PARAMETRISED BY  DIR
. *   NO CHECK FOR NEGATIVE SQRT, FLOATING POINT OVERFLOW OR DIVIDE FAULT
. *   DIR IS SUPPOSED TO BE TIME-LIKE
. *   POUT=PIN IF DIR=0
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
 
SLATE     INFO  2  2
$(2)
LORB      RES       1
SAVARG    RES       3
 
$(0)
          ON    FORE
LORA      +         'LOREN4'           . WALK BACK PACKET FOR 11A
DYN$      +         0
          OFF
 DO FTN ,LORARG  +  LORB
 
$(1),FIRS01
LOREN4*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A1,1,XARG          . LOC(PIN), LOC(POUT)
          L         A3,0,XARG          . LOC(DIR)
          DS        A1,SAVARG+1
          L         A4,2,A3            . D3
          DL        A0,0,A3            . D1, D2
          FM        A4,A4              . D3**2
          FM        A1,A1              . D2**2
          S         A3,SAVARG
          L         A2,3,A3            . D4
          FA        A4,A1              . D2**2+D3**2
          FM        A0,A0              . D1**2
          FM        A2,A2              . D4**2
          FA        A4,A0              . (3 VECT DIR)**2
          S         X11,DYN$
          FAN       A2,A4             . D4**2 - (D1**2+D2**2+D3**2)
 DO FTN , L,U       A0,LORARG
          SMA       A2,LORB
          ON    FORE++FORT**OPTR
LMJSQ1    LMJ       X11,SQRT
          +         LORB
 DO FORE , +        $-LOREN4-2,LORA
 DO NNOP , NOP
          OFF . FORE++FORT**OPTR
          ON    FORT**(-OPTR)
          L,U       A0,LORB
          LXI,U     X11,RMATH$
LMJSQ1    LIJ       X11,SQRT$
          OFF . FORT**(-OPTR)
          ON    FTN
LMJSQ1    LMJ       X11,SQRT
          OFF . FTN
          DL        A1,SAVARG+1         . LOC(PIN), LOC(POUT)
          L         X11,DYN$
          JZ        A0,LOR60
          S         A0,R1              . M
          DS        A8,A15+1
          L         A0,SAVARG          . LOC(DIR)
          DL        A3,0,A1            . P1, P2
          L         A8,2,A0            . D3
          DS        A6,R2
          FM        A8,2,A1            . D3*P3
          FM        A4,1,A0            . P2*D2
          FA        A8,A4              . D3*P3+D2*P2
          L         A6,3,A1            . P4
          FM        A3,0,A0            . P1*D1
          FM        A6,3,A0            . P4*D4
          FA        A8,A3              . (3VECT DIR)*(3VECT PIN)
          L         A3,3,A0           . D4
          FAN       A6,A8              . D4*P4-D*P
          FA        A3,R1              . D4+M
          FD        A6,R1              . POUT(4)
          FD        A8,A3              . (D*P)/(E+M)
          S         A6,3,A2            .
          FAN       A8,3,A1            . D*P/(E+M)-P4
          FD        A8,R1              . X=(D*P/(E+M)-P4)/M
          L         A3,0,A0            . D1
          L         A5,1,A0            . D2
          FM        A3,A8              . D1*X
          FM        A5,A8              . D2*X
          FM        A8,2,A0            . X.D3
          FA        A3,0,A1            . D1*X+P1
          FA        A5,1,A1           .  D2*X+P2
          FA        A8,2,A1             . D3*X+P3
          S         A3,0,A2            . POUT(1)
          DL        A6,R2
          S         A5,1,A2            . POUT(2)
          S         A8,2,A2            . POUT(3)
          DL        A8,A15+1           .
          J         RETUR3
LOR60                                  . POUT=PIN IF DIR=0
          L,U       R1,4
          LXI,U     A1,1
          LXI,U     A2,1
          BT        A2,0,*A1
          J         RETUR3
 
.     DIAGNOSTIC PACKETS FOR FORT
          ON    FORT++FTN
#include "gerr_c.inc"
          RES       1
          OFF
          ON    FORT
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,LOREN4
          +         0,DYN$
                    'LOREN4'
 DO OPTR ,DIAG2  +  0776202,LMJSQ1+1
 DO OPTR=0 ,DIAG2 + 0777302,LMJSQ1+1
          +         LMJSQ1-FIRS01,DIAG1
          OFF
          END
#ifdef CERNLIB_TCGEN_LOREN4
#undef CERNLIB_TCGEN_LOREN4
#endif
