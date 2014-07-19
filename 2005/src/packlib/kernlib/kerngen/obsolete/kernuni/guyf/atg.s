*
* $Id: atg.s,v 1.1.1.1 1996/02/15 17:54:02 mclareni Exp $
*
* $Log: atg.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:02  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     FUNCTION  ATG( RSIN, RCOS )                *** B 101 ***
.     THE RESULT IS IN THE RANGE 0,2PI  WHILE THE RESULT OF ATAN2 IS
.     IN THE RANGE -PI,+PI
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
          ON    FORT++FTN
#include "gerr_c.inc"
SAVX11    RES       1
          OFF .
          ON    FTN
SLATE     INFO  2  2
$(2),ATG1 RES  2
          OFF . FTN
$(0)
          ON    FORE
ATGA      'ATG   '
SAVX11    +         2,0
          OFF
PI2       +         6.2831853072
 
$(1),FIRS01
ATG*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A0,0,XARG
          S,H2      X11,SAVX11
          ON    FORE++FORT**OPTR
          DS        A0,ATG1
LMJATA    LMJ       X11,ATAN2
ATG1      RES  2
 DO FORE , +$-ATG-3,ATGA
 DO FORT**NNOP , NOP
          OFF . FORE++FORT**OPTR
          ON    FORT**(-OPTR)
          LXI,U     X11,RMATH$
LMJATA    LIJ       X11,ATAN2$
          OFF . FORT**(-OPTR)
          ON    FTN
          DS        A0,ATG1
          L,U       A0,ATG1
LMJATA    LMJ       X11,ATAN2
          OFF . FTN
          JP        A0,ATG2
          FA        A0,PI2
ATG2      L         X11,SAVX11
          J         RETUR2
 
. ------- DIAGNOSTICS PACKETS FOR  FORT ----------
          ON    FORT
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,ATG
          +         0,DYN$
                    'ATG'
 DO OPTR ,DIAG2  +  0776202,LMJATA+1
 DO OPTR=0 ,DIAG2  + 0777302,LMJATA+1
          +   LMJATA-FIRS01,DIAG1
          OFF
          END
#ifdef CERNLIB_TCGEN_ATG
#undef CERNLIB_TCGEN_ATG
#endif
