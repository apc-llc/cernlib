*
* $Id: cbyt.s,v 1.1.1.1 1996/02/15 17:53:59 mclareni Exp $
*
* $Log: cbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:59  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     SUBROUTINE CBYT(A,LA,X,LX,NBITS)           *** M 421 ***
.     COPIES THE BYTE AT LA OF A INTO THE BYTE AT LX OF X
. *   DOES NOT CHECK LA, LX, NBITS VALUES
. *   SEE JBIT
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1)
CBYT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG
          L         A4,0,A2            . A
          DL        A1,2,XARG
          LNA       A3,0,A3            . -LA
          L         A0,4,XARG
          L         A5,0,A1            . X
          LNA       A2,0,A2            . -LX
          LSSC      A4,37,A3           . THE BYTE AT LA OF A IS RIGHT ADJUSTED
          L         A0,0,A0            . NBITS
          LSSC      A5,37,A2           . THE BYTE AT LX OF X IS RIGHT ADJUSTED
          AN        A2,A0              . -LX -NBITS
          DSC       A4,0,A0            . INTRODUCE NEW BYTE AT LEFT
          SSC       A5,37,A2
          S         A5,0,A1            . NEW X
          J         RETUR5
          END
#ifdef CERNLIB_TCGEN_CBYT
#undef CERNLIB_TCGEN_CBYT
#endif
