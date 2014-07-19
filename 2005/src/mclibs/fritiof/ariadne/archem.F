*
* $Id: archem.F,v 1.1.1.1 1996/01/11 14:05:16 mclareni Exp $
*
* $Log: archem.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:16  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: archem.F,v 1.1.1.1 1996/01/11 14:05:16 mclareni Exp $
 
      SUBROUTINE ARCHEM(IMOD)
 
C...ARiadne subroutine CHEck Momentum conservation
 
C...Checks that momentum is conserved in ariadne
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),IEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),IPART
      SAVE /ARPART/
 
      COMMON /ARINT3/ DPTOT(5)
      SAVE /ARINT3/
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
      COMMON /ARJETX/ N,K(300,5),P(300,5),V(300,5)
      SAVE /ARJETX/
 
      DIMENSION DTOT(5)
 
 
C...Reset momentum counter. Include Drell-Yan produced particle
C...if present and check its momentum consistency.
      IF(MSTA(23).GT.0) THEN
        I=MSTA(23)
        DO 100 J=1,4
          DTOT(J)=P(I,J)
 100    CONTINUE
        IF(ABS(P(I,4)**2-P(I,3)**2-P(I,2)**2-P(I,1)**2-P(I,5)**2)
     $         .GT.PARA(39)*P(I,4)**2) CALL ARERRM('ARCHEM',10,I)
      ELSE
        DO 110 J=1,4
          DTOT(J)=0.0D0
 110    CONTINUE
      ENDIF
 
C...Sum all partons momentum and check their momentum concistency.
      DO 120 I=1,IPART
        DO 130 J=1,4
          DTOT(J)=DTOT(J)+BP(I,J)
 130    CONTINUE
        IF(ABS(BP(I,4)**2-BP(I,3)**2-BP(I,2)**2-BP(I,1)**2-BP(I,5)**2)
     $       .GT.PARA(39)*BP(I,4)**2.AND.MSTA(9).GE.2)
     $       CALL ARERRM('ARCHEM',10,I+N)
 120  CONTINUE
      DTOT(5)=DSQRT(DTOT(4)**2-DTOT(3)**2-DTOT(2)**2-DTOT(1)**2)
 
C...If IMOD=1 save total momentum for later use
      IF(IMOD.EQ.1) THEN
        DO 200 J=1,5
          DPTOT(J)=DTOT(J)
 200    CONTINUE
        RETURN
      ENDIF
 
C...IF IMOD=1 compare total momentum with old one
      DO 300 J=1,5
        IF(ABS(DTOT(J)-DPTOT(J)).GT.DPTOT(5)*PARA(39))
     $       CALL ARERRM('ARCHEM',9,0)
 300  CONTINUE
 
      RETURN
 
C**** END OF ARCHEM ****************************************************
      END
