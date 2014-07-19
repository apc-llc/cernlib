*
* $Id: v137m.F,v 1.1.1.1 1996/04/01 15:01:30 mclareni Exp $
*
* $Log: v137m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:30  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE V137M
C
      CHARACTER*(*) CODE
      PARAMETER (CODE='V137')
C
C     This Subroutine tests the MATHLIB routine V137 RNBNML
CSEQ,IMP64.
#include "iorc.inc"
C     V137   RNBNML
 
      LOGICAL LOK
 
      DIMENSION NT(11,9)
 
      DATA (NT( 1,I),I=1,9) /  0,  1,  1,  0,  0,  2,  1,  1,  2/
      DATA (NT( 2,I),I=1,9) /  1,  4,  5,  6,  7,  7, 11,  7,  8/
      DATA (NT( 3,I),I=1,9) /  0, 10,  5,  7,  6, 11, 16, 19, 20/
      DATA (NT( 4,I),I=1,9) /  3,  3,  7, 14, 13, 22, 25, 25, 29/
      DATA (NT( 5,I),I=1,9) /  4,  7, 10, 17, 25, 24, 30, 28, 33/
      DATA (NT( 6,I),I=1,9) /  4, 11, 21, 19, 34, 36, 36, 43, 48/
      DATA (NT( 7,I),I=1,9) /  4,  6, 16, 25, 36, 36, 47, 47, 54/
      DATA (NT( 8,I),I=1,9) /  8, 10, 26, 26, 30, 38, 49, 56, 62/
      DATA (NT( 9,I),I=1,9) /  8, 14, 27, 35, 44, 46, 69, 69, 73/
      DATA (NT(10,I),I=1,9) /  9, 22, 29, 45, 50, 59, 62, 74, 78/
      DATA (NT(11,I),I=1,9) /  8, 21, 37, 35, 44, 58, 74, 80, 94/
 
      CALL HEADER(CODE,1)

      CALL RLUXGO(3,314159265,0,0)
 
      LOK=.TRUE.
      DO 2 J = 1,11
      M=10*(J-1)+2
      DO 1 I = 1,9
      P=0.1*I
      CALL RNBNML(M,P,N,IERR)
    1 LOK=LOK .AND. N .EQ. NT(J,I)
    2 CONTINUE
      IRC= ITEST(CODE,LOK)
      CALL PAGEND(CODE)
      RETURN
      END
