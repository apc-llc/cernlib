*
* $Id: xzclos.F,v 1.1.1.1 1996/03/08 15:44:29 mclareni Exp $
*
* $Log: xzclos.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:29  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZCLOS(LUN,CHOPT,IRC)
#include "cspack/czsock.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
      CHARACTER*1   CHOPTE
#include "cspack/czopts.inc"
*
*     Close remote unit. CHOPT=D -> STATUS='DELETE'
*
      IRC = 0
      IF(IOPTC.NE.0) GOTO 20
      CALL CZSWAP(' ',LUN,ISTAT)
      CHOPTE = ' '
      IF(IOPTD.NE.0) CHOPTE = 'D'
      WRITE(CHMAIL,9001) LUN,CHOPTE
9001  FORMAT('XZIO :CLOS',I3,'/',A8,'/')
      CALL CZPUTA(CHMAIL,ISTAT)
#include "cspack/czmess.inc"
*
*     Clear node, user name and socket numbers
*
20    CONTINUE
      CHNODE(LUN) = ' '
      CHUSER(LUN) = ' '
      JSKIN(LUN)  = 0
      JSKOUT(LUN) = 0
      CALL CZSWAP(' ',0,ISTAT)
 
      END
 
