*
* $Id: jrsbyt.s,v 1.1.1.1 1996/02/15 17:54:00 mclareni Exp $
*
* $Log: jrsbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:00  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     FUNCTION JRSBYT(A,X,LX,NBITS)              *** M 421 ***
.     READS AND RESET THE BYTE AT LX OF X
.     IX = JRSBYT(...) IS IDENTICAL TO
.      IX = JBYT(X,LX,NBITS) FOLLOWED BY
.     CALL SBYT(A,X,LX,NBITS)
.     RETURNS THE BYTE AT LX OF X AND REPLACE (IN LOCATION X)
.     THIS BYTE BY THE BYTE AT 1 IN A
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1)
JRSBYT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A1,2,XARG          . LOC(LX), LOC(NBITS)
          S         A6,R1              . 7 REGISTERS NEEDED
          LNMA      A2,0,A2            . -NBITS
          ANU       A2,0,A1            . -LX-NBITS
          DL        A0,0,XARG          . LOC(A), LOC(X)
          L         A4,0,A1            . X
          L         A6,0,A0            . A
          LSSC      A4,37,A3
          DSL       A4,36,A2
          LDSC      A5,36,A2
          L         A0,A4              . BYTE(X)
          SSC       A6,37,A3
          S         A6,0,A1            . NEW  X
          J         RETUR4
          END
#ifdef CERNLIB_TCGEN_JRSBYT
#undef CERNLIB_TCGEN_JRSBYT
#endif
