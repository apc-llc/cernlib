*
* $Id: xzgetl.F,v 1.1.1.1 1996/03/08 15:44:30 mclareni Exp $
*
* $Log: xzgetl.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZGETL(LUN,CHLINE,CHFORM,CHOPT,IRC)
      CHARACTER*(*) CHLINE,CHFORM
#include "cspack/zmach.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
#include "cspack/czsock.inc"
#include "cspack/czopts.inc"
*
      IRC  = 0
*
*     Instruct server to issue a read
*
      CALL CZSWAP(' ',LUN,ISTAT)
      WRITE(CHMAIL,9001) LUN,CHFORM
9001  FORMAT('XZIO :GETL',I3,A)
      CALL CZPUTA(CHMAIL,ISTAT)
*
*     Get status information back from server
*
#include "cspack/czmess.inc"
      CHLINE = ' '
      IF(IRC.EQ.0) CALL CZGETA(CHLINE,IRC)
 
      END
