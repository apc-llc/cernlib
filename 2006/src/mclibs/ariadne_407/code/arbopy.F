*
* $Id: arbopy.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arbopy.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arbopy.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE ARBOPY(THEPY,PHIPY,DBXPY,DBYPY,DBZPY,PHI2PY)
 
C...ARiadne subroutine BOost to center of mass of PYthia event
 
C...Boost partons to the total CMS of a PYTHIA event
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
      COMMON /PYPARS/ MSTP(200),PARP(200),MSTI(200),PARI(200)
      SAVE /PYPARS/
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,XY,W2,XQ2,U
      SAVE /LEPTOU/
 
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
