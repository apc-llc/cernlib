*
* $Id: xzputl.F,v 1.1.1.1 1996/03/08 15:44:31 mclareni Exp $
*
* $Log: xzputl.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:31  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZPUTL(LUN,CHLINE,CHFORM,CHOPT,IRC)
      CHARACTER*(*) CHLINE,CHFORM
#include "cspack/zmach.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
#include "cspack/czsock.inc"
#include "cspack/czopts.inc"
*
      IRC  = 0
*
*     Instruct server to issue a write
*
      CALL CZSWAP(' ',LUN,ISTAT)
      WRITE(CHMAIL,9001) LUN,CHFORM
9001  FORMAT('XZIO :PUTL',I3,A)
      CALL CZPUTA(CHMAIL,ISTAT)
      CALL CZPUTA(CHLINE,IRC)
#include "cspack/czmess.inc"
 
      END
