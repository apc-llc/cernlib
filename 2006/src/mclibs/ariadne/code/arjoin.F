C***********************************************************************
C $Id: arjoin.F,v 1.2 1996/04/10 12:33:20 mclareni Exp $

      SUBROUTINE ARJOIN(J1,J2,J3)

C...ARiadne subroutine JOIN jets

C...Join three jets into two

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "ludat1.f"

C...Boost to CMS of jets
      DE=BP(J1,4)+BP(J2,4)+BP(J3,4)
      DBEX=(BP(J1,1)+BP(J2,1)+BP(J3,1))/DE
      DBEY=(BP(J1,2)+BP(J2,2)+BP(J3,2))/DE
      DBEZ=(BP(J1,3)+BP(J2,3)+BP(J3,3))/DE

      IF (DBEX**2+DBEY**2+DBEZ**2.GE.1.0D0) THEN
        BP(J1,1)=BP(J1,1)+BP(J2,1)*0.5
        BP(J1,2)=BP(J1,2)+BP(J2,2)*0.5
        BP(J1,3)=BP(J1,3)+BP(J2,3)*0.5
        BP(J1,4)=BP(J1,4)+BP(J2,4)*0.5
        BP(J1,5)=0.0
        BX1(J1)=0.0
        BP(J3,1)=BP(J3,1)+BP(J2,1)*0.5
        BP(J3,2)=BP(J3,2)+BP(J2,2)*0.5
        BP(J3,3)=BP(J3,3)+BP(J2,3)*0.5
        BP(J3,4)=BP(J3,4)+BP(J2,4)*0.5
        BP(J3,5)=0.0
        BX1(J3)=0.0
        QDONE(J2)=.TRUE.
        IDIPS=IDIPS-1
        RETURN
      ENDIF

      CALL AROBO3(0.0,0.0,-DBEX,-DBEY,-DBEZ,J1,J2,J3)

C...Rotate Jet 1 to z-axis and jet 2 to xz plane
      PX=BP(J1,1)
      PY=BP(J1,2)
      PZ=BP(J1,3)
      PHI=ULANGL(PX,PY)
      CALL AROBO3(0.0,-PHI,0.0D0,0.0D0,0.0D0,J1,J2,J3)
      THE=ULANGL(PZ,SQRT(PX**2+PY**2))
      CALL AROBO3(-THE,0.0,0.0D0,0.0D0,0.0D0,J1,J2,J3)
      PX=BP(J2,1)
      PY=BP(J2,2)
      PHI2=ULANGL(PX,PY)
      CALL AROBO3(0.0,-PHI2,0.0D0,0.0D0,0.0D0,J1,J2,J3)

C...Calculate energy fractions
      BE=BP(J1,4)+BP(J2,4)+BP(J3,4)
      B1=2.0*BP(J1,4)/BE
      B3=2.0*BP(J3,4)/BE

C...Determine recoil angle
      BET=ARANGL(J1,J3)
      PSI=0.0
      PSI=(PARU(1)-BET)*(B3**2)/(B1**2+B3**2)
      BP(J1,1)=0.0
      BP(J1,2)=0.0
      BP(J1,3)=BE*0.5
      BP(J1,4)=BE*0.5
      BP(J1,5)=0.0
      BX1(J1)=0.0
      BP(J3,1)=0.0
      BP(J3,2)=0.0
      BP(J3,3)=-BE*0.5
      BP(J3,4)=BE*0.5
      BP(J3,5)=0.0
      BX1(J3)=0.0
      QDONE(J2)=.TRUE.
      IDIPS=IDIPS-1

C...Rotate and boost back
      CALL AROBO2(PSI,0.0,0.0D0,0.0D0,0.0D0,J1,J3)
      CALL AROBO2(0.0,PHI2,0.0D0,0.0D0,0.0D0,J1,J3)
      CALL AROBO2(THE,PHI,DBEX,DBEY,DBEZ,J1,J3)

      RETURN

C**** END OF ARJOIN ****************************************************
      END
