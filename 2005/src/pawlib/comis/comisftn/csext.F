*
* $Id: csext.F,v 1.1.1.1 1996/02/26 17:16:50 mclareni Exp $
*
* $Log: csext.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:50  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 10/01/95  10.00.16  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSEXT(STR,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)
      CHARACTER *(*)STR
      INTEGER M(10)
#if !defined(CERNLIB_APOLLO)
      N=LEN(STR)
#endif
#if defined(CERNLIB_APOLLO)
      N=80
#endif
      NOPAR=1
      LEVEL=0
      DO 1 I=1,N
      IF(STR(I:I).EQ.'#')GO TO 3
      IF(STR(I:I).EQ.'(' )LEVEL=LEVEL+1
      IF(STR(I:I).EQ.')' )LEVEL=LEVEL-1
      IF(STR(I:I).EQ.',' .AND. LEVEL.EQ.0)NOPAR=NOPAR+1
  1   CONTINUE
  3   N=NOPAR
      M(1)=JUMPAD(P1)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(2)=JUMPAD(P2)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(3)=JUMPAD(P3)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(4)=JUMPAD(P4)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(5)=JUMPAD(P5)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(6)=JUMPAD(P6)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(7)=JUMPAD(P7)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(8)=JUMPAD(P8)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(9)=JUMPAD(P9)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(10)=JUMPAD(P10)
      N=N-1
      IF(N.EQ.0)GO TO 2
      WRITE(*,77)
 77   FORMAT(4X,'CSEXT: TOO MANY PARAMETERS')
      NOPAR=10
 2    CONTINUE
      CALL CSEXTX(STR,NOPAR,M)
      END
