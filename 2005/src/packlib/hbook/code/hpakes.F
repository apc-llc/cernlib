*
* $Id: hpakes.F,v 1.1.1.1 1996/01/16 17:07:45 mclareni Exp $
*
* $Log: hpakes.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:45  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          08/05/95  10.48.01  by  Julian Bunn
*-- Author : J.J.Bunn
      SUBROUTINE HPAKES(IDD,IX,IY,E)
*.==========>
*.           Fills histogram IDD's channel (IX,IY) with error E
*..=========> ( J.J.Bunn )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
      real E
*.___________________________________________
      CALL HFIND(IDD,'HPAKE ')
      IF(LCID.EQ.0)GO TO 99
      CALL HDCOFL
*
*   2-D histogram
*
      IF(I230.NE.0)THEN
         LW=LQ(LCONT)
         IF(LW.EQ.0)THEN
            CALL HBAR2(IDD)
            IF(IERR.NE.0)GO TO 99
            LW=LQ(LCONT)
         ENDIF
         NCX = IQ(LCID+KNCX)
         Q(LW+(IY-1)*NCX+IX) = E*E
         CALL SBIT0(IQ(LCID),5)
         GO TO 99
      ENDIF
*
*   1-D histogram
*
      IF(I9.EQ.0)THEN
         CALL HBARX(IDD)
         IF(IERR.NE.0)GO TO 99
      ENDIF
*
      CALL SBIT0(IQ(LCID),5)
      LW=LQ(LCONT)
      Q(LW+IX) = E*E
*
  99  RETURN
      END
