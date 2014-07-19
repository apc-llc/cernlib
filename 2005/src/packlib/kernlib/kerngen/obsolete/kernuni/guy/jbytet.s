*
* $Id: jbytet.s,v 1.1.1.1 1996/02/15 17:54:00 mclareni Exp $
*
* $Log: jbytet.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:00  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     FUNCTION JBYTET(A,X,LX,NBITS)              *** M 421 ***
.     RETURNS LOGICAL .AND. OF BYTE AT 1 IN A AND BYTE AT LX IN X
. *   SEE JBIT
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1)
JBYTET*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A1,2,XARG
          LNMA      A2,0,A2            . -NBITS
          ANU       A2,0,A1            . -LX-NBITS
          DL        A0,0,XARG
          L         A4,0,A1            . X
          LSSL      A4,37,A3
          SSL       A4,36,A2
          AND       A4,0,A0            . A.AND.BYTE(X)
          LA        A0,A5 .
          J         RETUR4
 END
#ifdef CERNLIB_TCGEN_JBYTET
#undef CERNLIB_TCGEN_JBYTET
#endif
