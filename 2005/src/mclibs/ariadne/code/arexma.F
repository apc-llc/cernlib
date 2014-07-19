C***********************************************************************
C $Id: arexma.F,v 1.2 1996/04/10 12:33:14 mclareni Exp $

      SUBROUTINE AREXMA(I1,I3)

C...ARiadne subroutine make EXtended partons MAssless

C...Makes extended partons massless.

#include "arimpl.f"
#include "arpart.f"
#include "ardat1.f"


      IF (MSTA(31).GT.0) RETURN
      IF ((.NOT.QEX(I1)).AND.(.NOT.QEX(I3))) RETURN
      DPE1=BP(I1,4)
      DPE3=BP(I3,4)
      DPE=DPE1+DPE3
      DPX1=BP(I1,1)
      DPX3=BP(I3,1)
      DBEX=(DPX1+DPX3)/DPE
      DPY1=BP(I1,2)
      DPY3=BP(I3,2)
      DBEY=(DPY1+DPY3)/DPE
      DPZ1=BP(I1,3)
      DPZ3=BP(I3,3)
      DBEZ=(DPZ1+DPZ3)/DPE
      CALL AROBO2(0.0,0.0,-DBEX,-DBEY,-DBEZ,I1,I3)

C...Calculate rotation angles but no need for rotation yet
      PX=BP(I1,1)
      PY=BP(I1,2)
      PZ=BP(I1,3)
      PHI=ULANGL(PX,PY)
      THE=ULANGL(PZ,SQRT(PX**2+PY**2))
      CALL AROBO2(0.0,-PHI,0.0D0,0.0D0,0.0D0,I1,I3)
      CALL AROBO2(-THE,0.0,0.0D0,0.0D0,0.0D0,I1,I3)
      IF (QEX(I1)) BP(I1,5)=0.0
      IF (QEX(I3)) BP(I3,5)=0.0
      BE=BP(I1,4)+BP(I3,4)
      BP(I1,4)=0.5*(BE**2+BP(I1,5)**2-BP(I3,5)**2)/BE
      BP(I3,4)=BE-BP(I1,4)
      BP(I1,3)=SQRT(BP(I1,4)**2-BP(I1,5)**2)
      BP(I3,3)=-BP(I1,3)
      BP(I1,2)=0.0
      BP(I3,2)=0.0
      BP(I1,1)=0.0
      BP(I3,1)=0.0

      CALL AROBO2(THE,PHI,DBEX,DBEY,DBEZ,I1,I3)

      RETURN

C**** END OF AREXMA ****************************************************
      END
