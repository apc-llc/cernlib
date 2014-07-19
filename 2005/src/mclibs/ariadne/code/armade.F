C***********************************************************************
C $Id: armade.F,v 1.2 1996/04/10 12:33:21 mclareni Exp $

      SUBROUTINE ARMADE

C...ARiadne subroutine set MAss DEpendencies

C...Sets some mass dependencies needed for ARMCDI

#include "arimpl.f"
#include "ardat1.f"
#include "arint1.f"


      SSY=SY1+SY2+SY3
      Y1=SY1**2
      Y2=SY2**2
      Y3=SY3**2

      BC1=DBLE(Y1)+1.0D0-DBLE(SY2+SY3)**2
      IF (IFLG.GT.0.AND.MSTA(23).EQ.2) BC1=DBLE(Y1)+1.0D0
      BC3=DBLE(Y3)+1.0D0-DBLE(SY2+SY1)**2
      XT2M=0.0
      IF (SQRT(0.25+Y2)-1.0+(BC1+BC3)/2.0.LT.0.0) RETURN
      XTS=(SQRT(0.25+Y2)-1.0+(BC1+BC3)/2.0)**2
      XT1=BC1-2.0*SY1
      XT3=BC3-2.0*SY3
      IF (XT1.LT.0.0) RETURN
      IF (XT3.LT.0.0) RETURN
      XT2M=MIN(XTS,XT1*XT3)

      BZP=0.5*(1.0+Y1-Y3+SQRT(1.0+(Y1-Y3)**2-2.0*(Y1+Y3)))
      BZM=0.5*(1.0+Y3-Y1+SQRT(1.0+(Y1-Y3)**2-2.0*(Y1+Y3)))

      RETURN

C**** END OF ARMADE ****************************************************
      END
