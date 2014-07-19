*
* $Id: sbit0.s,v 1.1.1.1 1996/02/15 17:54:00 mclareni Exp $
*
* $Log: sbit0.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:00  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     SUBROUTINE SBIT0(X,LX)                     *** M 421 ***
.     SETS 0 INTO BIT LX OF X
. *   SEE JBIT
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1)
SBIT0*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A1,0,XARG
          L         A5,0,A1            . X
          LNA       A2,0,A2            . -LX
          LSSC      A5,37,A2           . THE BIT LX IS RIGHT ADJUSTED
          SSL       A5,1               . BIT 0 INTRODUCED AT LEFT
          SSC       A5,36,A2
          S         A5,0,A1
          J         RETUR2
          END
#ifdef CERNLIB_TCGEN_SBIT0
#undef CERNLIB_TCGEN_SBIT0
#endif
