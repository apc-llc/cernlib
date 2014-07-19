*
* $Id: hdcofl.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: hdcofl.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.18/00 17/02/93  13.22.06  by  Rene Brun
*-- Author :
      SUBROUTINE HDCOFL
*.==========>
*.           DECODE BITS IN HISTOGRAM STATUS WORD
*..=========> ( R.Brun )
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
      DIMENSION IFLAG(37)
      EQUIVALENCE       (IFLAG(1),I1)
*.___________________________________________
      IF(IQ(LCID-2).NE.0)THEN
         DO 10 J=1,31
  10     IFLAG(J)=JBIT(IQ(LCID+KBITS),J)
      ELSE
         CALL VZERO(IFLAG,31)
      ENDIF
*
      I230=I2+I3
      I123=   I1+I230
      END
