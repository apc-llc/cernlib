*
* $Id: xzmv.F,v 1.1.1.1 1996/03/08 15:44:31 mclareni Exp $
*
* $Log: xzmv.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:31  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZMV(CHSRC,CHTGT,CHOPT,IRC)
#include "cspack/czsock.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
      CHARACTER*(*) CHSRC,CHTGT
      CHARACTER*8   CHOPTT
*
#include "cspack/czopts.inc"
#include "cspack/czopen.inc"
      IRC    = 0
      CHOPTT = ' '
      LOPT   = LENOCC(CHOPT)
      IF(LOPT.GT.0) CHOPTT = CHOPT(1:LOPT)
*
      CHMAIL = 'XZIO :MV /'//CHOPTT//'/'
      CALL CZPUTA(CHMAIL,ISTAT)
      LCHSRC = LENOCC(CHSRC)
      LCHTGT = LENOCC(CHTGT)
      CHMAIL = CHSRC(1:LCHSRC)
      CALL CZPUTA(CHMAIL,ISTAT)
      CHMAIL = CHTGT(1:LCHTGT)
      CALL CZPUTA(CHMAIL,ISTAT)
#include "cspack/czmess.inc"
*
   99 END
