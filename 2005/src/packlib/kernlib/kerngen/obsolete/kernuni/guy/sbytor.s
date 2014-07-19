*
* $Id: sbytor.s,v 1.1.1.1 1996/02/15 17:54:01 mclareni Exp $
*
* $Log: sbytor.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:01  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     SUBROUTINE SBYTOR(A,X,LX,NBITS)            *** M 421 ***
.     REPLACES THE BYTE AT LX OF X BY THE LOGICAL
.      .OR. OF THIS BYTE AND THE BYTE AT 1 OF A
. *   SEE JBIT
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1)
SBYTOR*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,2,XARG
          DL        A0,0,XARG
          LN        A2,0,A2            . -LX
          L         A4,0,A1            . X
          L         A3,0,A3            . NBITS
          LSSC      A4,37,A2           . THE BYTE AT LX IS RIGHT ADJUSTED
          OR         A4,0,A0           . .OR. WITH A
          AN        A2,A3
          DSC       A4,0,A3            . INTRODUCE RESULT ST LEFT
          SSC       A4,37,A2
          S         A4,0,A1
          J         RETUR4
          END
#ifdef CERNLIB_TCGEN_SBYTOR
#undef CERNLIB_TCGEN_SBYTOR
#endif
