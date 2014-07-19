C***********************************************************************
C $Id: arthrw.F,v 1.2 1996/04/10 12:33:41 mclareni Exp $

      REAL FUNCTION ARTHRW(ID,JRAD,I1,I3,IN1,IN2)

C...ARiadne subroutine THRoW emission

C...Signals emissions not fulfilling certain criteria

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arhide.f"


      UTHROW=ARUTHR(ID,JRAD,I1,I3,IN1,IN2)
      ARTHRW=UTHROW
      IF (PARA(28).EQ.0.0.OR.UTHROW.LT.0) RETURN
      ARTHRW=-1.0
      GMAX=PARA(40)
      GMIN=0.0
      IF (PARA(28).GT.0.0) THEN
        GMIN=PARA(28)
      ELSE
        GMAX=-PARA(28)
      ENDIF
      DO 100 IP=IN1,IN2
        IF (IFL(IP).EQ.21.AND.(BP(IP,4).GT.GMAX.OR.BP(IP,4).LT.GMIN))
     $       RETURN
 100  CONTINUE

      ARTHRW=1.0

      RETURN

C**** END OF ARTHRW ****************************************************
      END
