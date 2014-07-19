*
* $Id: znumid.F,v 1.2 1999/06/18 13:31:24 couet Exp $
*
* $Log: znumid.F,v $
* Revision 1.2  1999/06/18 13:31:24  couet
* - qcardl.inc was empty: It is now removed and not used.
*   Comment END CDE removed.
*
* Revision 1.1.1.1  1996/03/06 10:47:15  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      SUBROUTINE ZNUMID (ID,NID)

#include "zebra/quest.inc"
*
      DIMENSION    ID(9), NID(9)


      NAME = ID(1)
      IF (IABS(NAME).GE.10000)     GO TO 22
      CALL VBLANK (IQUEST(26),4)
      CALL USET (NAME,IQUEST(26),1,4)
      CALL UBUNCH (IQUEST(26),NAME,4)
   22 NID(1) = NAME
      RETURN
      END
