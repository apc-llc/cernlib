*
* $Id: iuchan.s,v 1.1.1.1 1996/02/15 17:53:59 mclareni Exp $
*
* $Log: iuchan.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:59  mclareni
* Kernlib
*
*
. **************************************
.
.         FUNCTION IUCHAN( X, XLOW, DX, NX )     *** Y 201 ***
. *       DOES NOT CHECK DX.GT.0)  , NX.GT.0
. *       ASSUMES NX.LT.2**27
.
. **************************************
 
          AXR$
          FORT$
$(1)
IUCHAN*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(X), LOC(XLOW)
          DL        A0,2,XARG          . LOC(DX), LOC(NX)
          L         A4,0,A2            . X
          FAN       A4,0,A3            . X-XLOW
          FD        A4,0,A0            . (X-XLOW)/DX
          JN        A4,UNDFL           . X.LT.XLOW
          LUF       A4,A4
          L         A0,0,A1            . NX
          TG,U      A4,0234
          J         OVFL               . IUCHAN.GE.2**27
          TLE,U     A4,0201
          L,U       A5,0
          LN        A3,A4
          SSA       A5,0233,A3         .
          TG        A5,A0
          J         OVFL
          L         A0,A5
OVFL      A,U       A0,1
          J         RETUR4
UNDFL     L,U       A0,0
          J         RETUR4             . RETURN
          END
#ifdef CERNLIB_TCGEN_IUCHAN
#undef CERNLIB_TCGEN_IUCHAN
#endif
