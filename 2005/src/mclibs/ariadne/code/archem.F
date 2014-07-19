C***********************************************************************
C $Id: archem.F,v 1.2 1996/04/10 12:33:03 mclareni Exp $

      SUBROUTINE ARCHEM(IMOD)

C...ARiadne subroutine CHEck Momentum conservation

C...Checks that momentum is conserved in ariadne

#include "arimpl.f"
#include "arpart.f"
#include "arint3.f"
#include "ardat1.f"
#include "lujets.f"

      DIMENSION DTOT(5)


C...Reset momentum counter. Include Drell-Yan produced particle and
C...others in special positions if present and check its momentum
C...consistency.
      DO 100 J=1,4
        DTOT(J)=0.0D0
 100  CONTINUE
      DO 110 I=MAXPAR-4,MAXPAR-2
        IF (.NOT.QQ(I)) GOTO 110
        DO 120 J=1,4
          DTOT(J)=DTOT(J)+BP(I,J)
 120    CONTINUE
        IF (ABS(BP(I,4)**2-BP(I,3)**2-BP(I,2)**2-BP(I,1)**2-BP(I,5)**2)
     $         .GT.PARA(39)*BP(I,4)**2) CALL ARERRM('ARCHEM',10,I)
 110  CONTINUE
      
C...Sum all partons momentum and check their momentum concistency.
      DO 130 I=1,IPART
        DO 140 J=1,4
          DTOT(J)=DTOT(J)+BP(I,J)
 140    CONTINUE
        IF (ABS(BP(I,4)**2-BP(I,3)**2-BP(I,2)**2-BP(I,1)**2-BP(I,5)**2)
     $       .GT.PARA(39)*BP(I,4)**2.AND.MSTA(9).GE.2)
     $       CALL ARERRM('ARCHEM',10,I+N)
 130  CONTINUE
      DTOT(5)=DSQRT(MAX(DTOT(4)**2-DTOT(3)**2-DTOT(2)**2-DTOT(1)**2,
     $                  0.0D0))

C...If IMOD=1 save total momentum for later use
      IF (IMOD.EQ.1) THEN
        DO 200 J=1,5
          DPTOT(J)=DTOT(J)
 200    CONTINUE
        RETURN
      ENDIF

C...IF IMOD=1 compare total momentum with old one
      DIFF=0.0D0
      DO 300 J=1,5
        DIFF=DIFF+(DTOT(J)-DPTOT(J))**2
 300  CONTINUE
      IF (DIFF.GT.(DPTOT(5)*PARA(39))**2) CALL ARERRM('ARCHEM',9,0)

      RETURN

C**** END OF ARCHEM ****************************************************
      END
