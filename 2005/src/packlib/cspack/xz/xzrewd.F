*
* $Id: xzrewd.F,v 1.1.1.1 1996/03/08 15:44:32 mclareni Exp $
*
* $Log: xzrewd.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:32  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZREWD(LUN,CHOPT,IRC)
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
#include "cspack/czopts.inc"
      IRC = 0
*
*     Request rewind of LUN
*
      CALL CZSWAP(' ',LUN,ISTAT)
      WRITE(CHMAIL,9001) LUN
9001  FORMAT('XZIO :REWD',I3)
      CALL CZPUTA(CHMAIL,ISTAT)
*  Sequence to process server messages
#include "cspack/czmess.inc"
      CALL CZSWAP(' ',0,ISTAT)
 
      END
 
