*
* $Id: arupdj.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: arupdj.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arupdj.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
 
      SUBROUTINE ARUPDJ(I2,I1,I3)
 
C...ARiadne subroutine UPDate Jet entry
 
C...Takes a jet entry I2 and determines its minimum invariant pt wrt.
C...all other jets. I1 and I3 indicates which jets have been changed
C...since last call
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
      COMMON /ARJETX/ N,K(300,5),P(300,5),V(300,5)
      SAVE /ARJETX/
 
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
 
 
      IF(K(I2,5).LT.0) RETURN
      IF(I1.EQ.0.OR.K(K(I2,3),5).LT.0.OR.K(K(I2,4),5).LT.0.OR.
     $     I2.EQ.I1.OR.I2.EQ.I3.OR.K(I2,3).EQ.I1.OR.K(I2,4).EQ.I3) THEN
        V(I2,5)=PARA(40)
        DO 100 J1=N+1,N+MSTU(3)-1
          IF(K(J1,5).LT.0) GOTO 100
          IF(J1.EQ.I2) GOTO 100
          DO 110 J3=J1+1,N+MSTU(3)
            IF(K(J3,5).LT.0) GOTO 110
            IF(J3.EQ.I2) GOTO 110
            CALL ARSMPT(J1,I2,J3)
 110      CONTINUE
 100    CONTINUE
      ELSE
        DO 200 J=N+1,N+MSTU(3)
          IF(J.EQ.I2.OR.K(J,5).LT.0) GOTO 200
          IF(J.GT.I1) CALL ARSMPT(I1,I2,J)
          IF(J.LT.I3) CALL ARSMPT(J,I2,I3)
          IF(J.LT.I1) CALL ARSMPT(J,I2,I1)
          IF(J.GT.I3) CALL ARSMPT(I3,I2,J)
 200    CONTINUE
      ENDIF
 
      RETURN
 
C**** END OF ARUPDJ ****************************************************
      END
