*
* $Id: jbyt.s,v 1.1.1.1 1996/02/15 17:54:00 mclareni Exp $
*
* $Log: jbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:00  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     FUNCTION JBYT(A,LA,NBITS)                  *** M 421 ***
.     RETURNS THE BYTE AT LA OF A
. *   SEE JBIT
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1)
JBYT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A1,1,XARG          . LOC(LA), LOC(NBITS)
          L         A0,0,XARG          . LOC(A)
          LNMA      A2,0,A2            . -NBITS
          ANU       A2,0,A1            . -LA-NBITS
          L         A0,0,A0            . A
          LSSL      A0,37,A3 .    37-LA-NBITS
          SSL       A0,36,A2 .    36-NBITS
          J         RETUR3
 END
#ifdef CERNLIB_TCGEN_JBYT
#undef CERNLIB_TCGEN_JBYT
#endif
