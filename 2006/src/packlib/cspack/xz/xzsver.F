*
* $Id: xzsver.F,v 1.1.1.1 1996/03/08 15:44:32 mclareni Exp $
*
* $Log: xzsver.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:32  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZSVER
#include "cspack/czsock.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
*
#include "cspack/czopen.inc"
      CHMAIL = 'XZIO :VER'
      CALL CZPUTA(CHMAIL,ISTAT)
#include "cspack/czmess.inc"
  99  END
 
