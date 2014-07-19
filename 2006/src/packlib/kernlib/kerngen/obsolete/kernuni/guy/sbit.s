*
* $Id: sbit.s,v 1.1.1.1 1996/02/15 17:54:00 mclareni Exp $
*
* $Log: sbit.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:00  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     SUBROUTINE SBIT(A,X,LX)                    *** M 421 ***
.     COPIES BIT 1 OF A INTO BIT LX OF X
. *   SEE JBIT
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1)
SBIT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A1,1,XARG          . LOC(X), LOC(LX)
          L         A3,0,XARG          . LOC(A)
          LNA       A2,0,A2            . -LX
          L         A5,0,A1            . X
          L         A4,0,A3            . A
          LSSC      A5,37,A2           . THE BIT LX IS RIGHT ADJUSTED
          DSL       A4,1               . NEW BIT INTRODUCED AT LEFT
          SSC       A5,36,A2
          S         A5,0,A1
          J         RETUR3
          END
#ifdef CERNLIB_TCGEN_SBIT
#undef CERNLIB_TCGEN_SBIT
#endif
