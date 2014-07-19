*
* $Id: fnnum.F,v 1.1.1.1 1996/02/15 17:52:35 mclareni Exp $
*
* $Log: fnnum.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:35  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      INTEGER FUNCTION FNNUM (NAMEP)
C
C CERN PROGLIB# Z204    FNNUM           .VERSION KERNCRY  1.00  830612
C ORIG. 12/06/83 JZ
C

      NAME = NAMEP
      IF (SHIFTR(NAME,8).NE.0)   GO TO 29
      IF (NAME.LT.100)           GO TO 12
      IF (NAME.GE.103)           GO TO 29
      IF (NAME.EQ.100)  NAME=3L$IN
      IF (NAME.EQ.101)  NAME=4L$OUT
      IF (NAME.EQ.102)  NAME=6L$PUNCH
      GO TO 28

   12 IONE = MOD (NAME,10)
      ITEN = NAME/10

      NAME = 2LFT .OR. SHIFTL (SHIFTL(ITEN+48,8).OR.(IONE+48),32)

   28 NAMEP = NAME
   29 FNNUM = NAME
      RETURN
      END
