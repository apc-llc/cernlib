*
* $Id: xzcd.F,v 1.1.1.1 1996/03/08 15:44:29 mclareni Exp $
*
* $Log: xzcd.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:29  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZCD(PATH,IRC)
#include "cspack/czsock.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
      CHARACTER*(*) PATH
*
      IRC    = 0
      LPATH  = LENOCC(PATH)
*
      CHMAIL = 'XZIO :CD  '//PATH(1:LPATH)
      CALL CZPUTA(CHMAIL,ISTAT)
#include "cspack/czmess.inc"
  99  END
 
