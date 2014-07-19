*
* $Id: simple.F,v 1.1.1.1 1996/04/01 15:03:16 mclareni Exp $
*
* $Log: simple.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:16  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE SIMPLE(A,B,C,Z0,MN,MD,IROW,ICOL,LCV,Q,MODE,IOPT)
C  =====================================================================
C  INTERFACE ROUTINE ARRANGING THE OUTPUT
C  THE VALUES OF THE VARIABLES AND SLACK-VARIABLES ARE STORED IN THE FIR
C  M+N LOCATIONS OF VECTOR Q
C  =====================================================================
C     IOPT = 0   NO PRINTED OUTPUT
C     IOPT = 1   OUTPUT OF THE RESULTS ON A NEW PAGE
C     IOPT = 2   OUTPUT OF THE RESULTS ON THE NEXT NEW HALF OF A PAGE
C     IOPT = 3   OUTPUT OF THE RESULTS ON THE NEXT NEW THIRD OF A PAGE
C     IOPT = 4   OUTPUT OF THE RESULTS ON THE NEXT NEW QUARTER OF A PAGE
      DOUBLE PRECISION A, B, C, Z0, Q
      DIMENSION        A(MD,*), B(*), C(*), Q(*), IROW(*), ICOL(*),
     1                 LCV(*), MN(*), F(4)
      DATA F/1H1,1H4,1H5,1H6/
      M1 = MN(1)
      M  = MN(2)
      N1 = MN(3)
      N  = MN(4)
      LC = M+1
      LCQ = LC+N
      IQ = M+N+1
      CALL SPLX (A,B,C,Z0,M1,M,N1,N,MD,IROW,ICOL,LCV,LCV(LC),LCV(LCQ),
     1           Q,Q(IQ),MODE)
      IF(IOPT.LE.0 .OR. IOPT.GT.4) GO TO 5
      WRITE(6,100)F(IOPT)
      IF(MODE .EQ. 0)  WRITE(6,110)
      IF(MODE .EQ. 1)  WRITE(6,111)
      IF(MODE .EQ. 2)  WRITE(6,112)
      IF(MODE .EQ. 3)  WRITE(6,113)
      IF(MODE .EQ. 1  .OR.  MODE .EQ. 2)  RETURN
    5 DO 10  I = 1, M
      II = IROW(I)
   10 Q(II) = 0.
      DO 20  K = 1, N
      II = ICOL(K)
   20 Q(II) = C(K)
      IF(IOPT .EQ. 0)  RETURN
      WRITE(6,120)
      IF(M-N)  30, 30, 40
   30 DO 35  I = 1, M
      II = I+M
   35 WRITE(6,125) I, Q(I), II, Q(II)
      IF(M .EQ. N)  GOTO 50
      M1 = M+1
      DO 37  I = M1, N
      II = I+M
   37 WRITE(6,126) II, Q(II)
      GOTO 50
   40 DO 45  I = 1, N
      II = I+M
   45 WRITE(6,125) I, Q(I), II, Q(II)
      N1 = N+1
      DO 47  I = N1, M
   47 WRITE(6,127) I, Q(I)
   50 WRITE(6,130) Z0
      RETURN
  100 FORMAT(A1/' SOLUTION OF THE MAXIMIZATION PROBLEM'/'0',120('-')//)
  110 FORMAT(' THE PROBLEM HAS EXACTLY ONE FINITE MAXIMUM SOLUTION'//)
  111 FORMAT(' THE PROBLEM HAS NO FINITE SOLUTION (MAX = INFINITE)')
  112 FORMAT(' NO FEASIBLE INITIAL SOLUTION FOR THE PROBLEM EXISTS')
  113 FORMAT(' MORE THAN ONE MAXIMUM SOLUTION EXISTS'//)
  120 FORMAT('0VARIABLES',41X,'SLACK-VARIABLES'//1X,2('INDEX',10X,
     +       'VALUE',30X)/)
  125 FORMAT(1X,2(I4,D26.15,20X))
  126 FORMAT(51X,I4,D26.15)
  127 FORMAT(1X,I4,D26.15)
  130 FORMAT('0MAXIMUM VALUE OF THE OBJECTIVE FUNCTION = ',D22.15)
      END
