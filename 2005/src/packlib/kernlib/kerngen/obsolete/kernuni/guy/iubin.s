*
* $Id: iubin.s,v 1.1.1.1 1996/02/15 17:53:59 mclareni Exp $
*
* $Log: iubin.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:59  mclareni
* Kernlib
*
*
. **************************************
.
.     FUNCTION IUBIN( X, PAR, SPILL )            *** Y 202 ***
.     FUNCTION IUHIST( X, PAR, SPILL )            *** Y 203 ***
.     PAR(1)=NCHAN,  PAR(2)=DX,  PAR(3)=XLOW
.     RETURNS IUBIN = (X-XLOW)/DX+1.    0 FOR UNDERFLOW, NCHAN+1 FOR OVERFLOW
.     SPILL = .FALSE.  WITHIN RANGE,   .TRUE. FOR UNDERFLOW AND OVERFLOW
. *   DOES NOT CHECK  PAR(2)>0,  PAR(1) POSITIVE INTEGER
. *   ASSUMES NCHAN.LT.2**27
.     IUHIST: SAME AS IUBIN EXCEPTED FOR PAR(2)=1./DX
.
. **************************************
 
          AXR$
          FORT$
$(1)
IUBIN*
          L,U       A3,0
IUB0
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A1,1,XARG          . LOC(PAR), LOC(SPILL)
          L         A0,0,XARG          . LOC(X)
          LN        A4,2,A1            . -XLOW
          FA        A4,0,A0            . X-XLOW
          EX        FDFM,A3            . (X-XLOW)/BINWIDTH
          JN        A4,UNDFL
          LUF       A4,A4              . CONVERT TO INTEGER
          L         A0,0,A1            . NX
          TG,U      A4,0234
          J         OVFL               . IUBIN.GE.2**27
          TLE,U     A4,0201
          L,U       A5,0               . IUBIN=1
          LN        A3,A4
          SSA       A5,0233,A3
          TG        A5,A0
          J         OVFL               . IUBIN.GT.NX
          L         A0,A5
          L,U       A1,0
IUB20
          A,U       A0,1
          S         A1,0,A2            . SPILL
          J         RETUR3
 
UNDFL     LN,U      A0,1
OVFL      L,U       A1,1
          J         IUB20
 
IUHIST*
          L,U       A3,1
          J         IUB0
 
FDFM      FD        A4,1,A1            . /DX
          FM        A4,1,A1            . *(1./DX)
          END
#ifdef CERNLIB_TCGEN_IUBIN
#undef CERNLIB_TCGEN_IUBIN
#endif
#ifdef CERNLIB_TCGEN_IUHIST
#undef CERNLIB_TCGEN_IUHIST
#endif
