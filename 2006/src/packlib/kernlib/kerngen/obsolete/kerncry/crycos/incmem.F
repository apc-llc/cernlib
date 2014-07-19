*
* $Id: incmem.F,v 1.1.1.1 1996/02/15 17:52:41 mclareni Exp $
*
* $Log: incmem.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:41  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION INCMEM (NINC)
C
C CERN PROGLIB# Z027    INCMEM          .VERSION KERNCRY  1.03  840215
C ORIG. 18/01/84  W.W. SACLAY/DPHPE
C                 NEW ROUTINE FOR CRAY SYSTEM  COS 1.13 / CFT 1.11
C
      N = NINC
      CALL MEMORY ('CURFL',MEMB)
      IF (N.NE.0)     CALL MEMORY ('UC',N)
      CALL MEMORY ('CURFL',MEMA)
      NINC   = MEMA-MEMB
      INCMEM = MEMA
      RETURN
      END
