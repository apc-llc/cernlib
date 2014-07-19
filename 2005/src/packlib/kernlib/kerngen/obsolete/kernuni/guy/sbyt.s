*
* $Id: sbyt.s,v 1.1.1.1 1996/02/15 17:54:01 mclareni Exp $
*
* $Log: sbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:01  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     SUBROUTINE SBYT(A,X,LX,NBITS)              *** M 421 ***
.     COPIES THE BYTE AT 1 OF A INTO THE BYTE AT LX OF X
. *   SEE JBIT
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1)
SBYT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,2,XARG          . LOC(LX), LOC(NBITS)
          DL        A0,0,XARG          . LOC(A), LOC(X)
          LN        A2,0,A2            . -LX
          L         A3,0,A3            . NBITS
          L         A5,0,A1            . X
          L         A4,0,A0            . A
          LSSC      A5,37,A2           . RIGHT ADJUST BYTE
          AN        A2,A3              . -LX-NBITS
          DSL       A4,0,A3            . INTRODUCE NEW BYTE AT LEFT
          SSC       A5,37,A2
          S         A5,0,A1
          J         RETUR4
 END
#ifdef CERNLIB_TCGEN_SBYT
#undef CERNLIB_TCGEN_SBYT
#endif
