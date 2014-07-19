CDECK  ID>, HWDTWO.
*CMZ :-        -27/01/94  17.38.49  by  Mike Seymour
*-- Author :    Bryan Webber & Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWDTWO(P0,P1,P2,PCM,COSTH,ZAXIS)
C-----------------------------------------------------------------------
C     GENERATES DECAY 0 -> 1+2
C
C     PCM IS CM MOMENTUM
C
C     COSTH = COS THETA IN P0 REST FRAME (>1 FOR ISOTROPIC)
C     IF ZAXIS=.TRUE., COS THETA IS MEASURED FROM THE ZAXIS
C     IF .FALSE., IT IS MEASURED FROM P0'S DIRECTION
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWRUNI,ONE,ZERO,PCM,COSTH,C,S,P0(5),P1(5),P2(5),
     & PP(5),R(9)
      LOGICAL ZAXIS
      EXTERNAL HWRUNI
      PARAMETER (ZERO=0.D0, ONE=1.D0)
C--CHOOSE C.M. ANGLES
      C=COSTH
      IF (C.GT.1.) C=HWRUNI(0,-ONE,ONE)
      S=SQRT(1.-C*C)
      CALL HWRAZM(PCM*S,PP(1),PP(2))
C--PP IS MOMENTUM OF 2 IN C.M.
      PP(3)=-PCM*C
      PP(4)=SQRT(P2(5)**2+PCM**2)
      PP(5)=P2(5)
C--ROTATE IF NECESSARY
      IF (COSTH.LE.1..AND..NOT.ZAXIS) THEN
        CALL HWUROT(P0,ONE,ZERO,R)
        CALL HWUROB(R,PP,PP)
      ENDIF
C--BOOST FROM C.M. TO LAB FRAME
      CALL HWULOB(P0,PP,P2)
      CALL HWVDIF(4,P0,P2,P1)
      END
