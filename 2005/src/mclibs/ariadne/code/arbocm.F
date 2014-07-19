C***********************************************************************
C $Id: arbocm.F,v 1.2 1996/04/10 12:33:02 mclareni Exp $

      SUBROUTINE ARBOCM(ID)

C...ARiadne subroutine BOost to Center of Mass

C...Boost the partons in dipole ID to the CMS of the dipole

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arint2.f"


C...Calculate boostvector and boost
      I1=IP1(ID)
      I3=IP3(ID)
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

      RETURN

C**** END OF ARBOCM ****************************************************
      END
C***********************************************************************
C $Id: arbocm.F,v 1.2 1996/04/10 12:33:02 mclareni Exp $

      SUBROUTINE ARBOLE(THEL,PHI1,PHI2,DBXL,DBYL,DBZL)

C...ARiadne subroutine BOost to hadronic center of mass of LEpto event

C...Boost partons to the hadronic CMS of a LEPTO event


#include "arimpl.f"
#include "lujets.f"


      DBXL=0.0D0
      DBYL=0.0D0
      DBZL=0.0D0
      DBEL=0.0D0
      THEL=0.0
      PHI1=0.0
      PHI2=0.0

      DO 100 I=5,N
        DBXL=DBXL+DBLE(P(I,1))
        DBYL=DBYL+DBLE(P(I,2))
        DBZL=DBZL+DBLE(P(I,3))
        DBEL=DBEL+DBLE(P(I,4))
 100  CONTINUE

      DBXL=DBXL/DBEL
      DBYL=DBYL/DBEL
      DBZL=DBZL/DBEL

      CALL LUDBRB(1,N,0.0,0.0,-DBXL,-DBYL,-DBZL)
      PHI1=ULANGL(P(3,1),P(3,2))
      THEL=ULANGL(P(3,3),SQRT(P(3,1)**2+P(3,2)**2))
        
      CALL LUDBRB(1,N,0.0,-PHI1,0.0D0,0.0D0,0.0D0)
      CALL LUDBRB(1,N,-THEL,0.0,0.0D0,0.0D0,0.0D0)

      PHI2=ULANGL(P(1,1),P(1,2))
      CALL LUDBRB(1,N,0.0,-PHI2,0.0D0,0.0D0,0.0D0)

      RETURN

C**** END OF ARBOLE ****************************************************
      END

C***********************************************************************
C $Id: arbocm.F,v 1.2 1996/04/10 12:33:02 mclareni Exp $

      SUBROUTINE ARBOPY(THEPY,PHIPY,DBXPY,DBYPY,DBZPY,PHI2PY)

C...ARiadne subroutine BOost to center of mass of PYthia event

C...Boost partons to the total CMS of a PYTHIA event


#include "arimpl.f"
#include "lujets.f"
#include "ludat1.f"
#include "pypars.f"
#include "leptou.f"

      DOUBLE PRECISION DBP(4)


      ISUB=MSTI(1)
      DO 10 J=1,4
        DBP(J)=0.0D0
 10   CONTINUE
     
      IF ((ISUB.EQ.10.OR.ISUB.EQ.83).AND.
     $     ABS(MSTI(15)).GE.11.AND.ABS(MSTI(15)).LE.18.AND.
     $     ABS(MSTI(16)).GE.1.AND.ABS(MSTI(16)).LE.8) THEN

        DO 100 I=MSTI(4),N
          IF (K(I,3).NE.2.AND.K(I,3).NE.MSTI(8)) GOTO 100
          DO 110 J=1,4
            DBP(J)=DBP(J)+DBLE(P(I,J))
 110      CONTINUE
 100    CONTINUE

        IQ=-2
        IL=MSTI(7)

        DBXPY=DBP(1)/DBP(4)
        DBYPY=DBP(2)/DBP(4)
        DBZPY=DBP(3)/DBP(4)

        X=PARI(34)
        XQ2=-PARI(15)
        W2=DBP(4)**2-DBP(3)**2-DBP(2)**2-DBP(1)**2
        U=P(2,4)*(P(IL-2,4)-P(IL,4))-P(2,3)*(P(IL-2,3)-P(IL,3))-
     $       P(2,2)*(P(IL-2,2)-P(IL,2))-P(2,1)*(P(IL-2,1)-P(IL,1))
        XY=U/(P(1,4)*P(2,4)-P(1,3)*P(2,3)-P(1,2)*P(2,2)-P(1,1)*P(2,1))

      ELSEIF ((ISUB.EQ.10.OR.ISUB.EQ.83).AND.
     $       ABS(MSTI(16)).GE.11.AND.ABS(MSTI(16)).LE.18.AND.
     $       ABS(MSTI(15)).GE.1.AND.ABS(MSTI(15)).LE.8) THEN

        DO 200 I=MSTI(4)+1,N
          IF (K(I,3).NE.1.AND.K(I,3).NE.MSTI(7)) GOTO 200
          DO 210 J=1,4
            DBP(J)=DBP(J)+DBLE(P(I,J))
 210      CONTINUE
 200    CONTINUE

        IQ=1
        IL=MSTI(8)

        DBXPY=DBP(1)/DBP(4)
        DBYPY=DBP(2)/DBP(4)
        DBZPY=DBP(3)/DBP(4)

        X=PARI(33)
        XQ2=-PARI(15)
        W2=DBP(4)**2-DBP(3)**2-DBP(2)**2-DBP(1)**2
        U=P(1,4)*(P(IL-2,4)-P(IL,4))-P(1,3)*(P(IL-2,3)-P(IL,3))-
     $       P(1,2)*(P(IL-2,2)-P(IL,2))-P(1,1)*(P(IL-2,1)-P(IL,1))
        XY=U/(P(1,4)*P(2,4)-P(1,3)*P(2,3)-P(1,2)*P(2,2)-P(1,1)*P(2,1))

      ELSE

        DEPY=DBLE(P(1,4))+DBLE(P(2,4))
        DBXPY=(DBLE(P(1,1))+DBLE(P(2,1)))/DEPY
        DBYPY=(DBLE(P(1,2))+DBLE(P(2,2)))/DEPY
        DBZPY=(DBLE(P(1,3))+DBLE(P(2,3)))/DEPY

        IQ=1
        IL=0

        XQ2=-1.0

      ENDIF

      CALL LUDBRB(1,N,0.0,0.0,-DBXPY,-DBYPY,-DBZPY)

      I=ABS(IQ)
      PHIPY=ULANGL(P(I,1),P(I,2))
      THEPY=ULANGL(P(I,3),SQRT(P(I,1)**2+P(I,2)**2))
      IF (IQ.LT.0) THEPY=PARU(1)+THEPY
        
      CALL LUDBRB(1,N,0.0,-PHIPY,0.0D0,0.0D0,0.0D0)
      CALL LUDBRB(1,N,-THEPY,0.0,0.0D0,0.0D0,0.0D0)

      PHI2PY=0.0
      IF (IL.GT.0) THEN
        PHI2PY=ULANGL(P(IL,1),P(IL,2))
        CALL LUDBRB(1,N,0.0,-PHI2PY,0.0D0,0.0D0,0.0D0)
      ENDIF

      RETURN

C**** END OF ARBOPY ****************************************************
      END
