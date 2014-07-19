*
* $Id: jbit.s,v 1.1.1.1 1996/02/15 17:54:00 mclareni Exp $
*
* $Log: jbit.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:00  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     FUNCTION JBIT(A,LA)                        *** M 421 ***
.     RETURNS BIT LA OF A
. *   RESTRICTIONS FOR ALL THE SHIFT ROUTINES OF PACKAGE
.     M 421, M 422, M 423, M 427, M 428
. *   NO CHECK IS DONE FOR THE VALUES OF LA,LX,NBITS
. *   ERRONEOUS VALUES WILL GIVE UNPREDICTABLE RESULTS
. *   SINGLE PRECISION A,X
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1)
JBIT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A1,0,XARG          . LOC(A), LOC(LA)
          LNMA      A2,0,A2            . -LA
          L         A0,0,A1            . A
          LSSL      A0,36,A2 .    36-LA
          SSL       A0,35 .
          J         RETUR2
          END
#ifdef CERNLIB_TCGEN_JBIT
#undef CERNLIB_TCGEN_JBIT
#endif
