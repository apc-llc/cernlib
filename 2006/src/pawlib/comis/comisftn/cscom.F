*
* $Id: cscom.F,v 1.1.1.1 1996/02/26 17:16:50 mclareni Exp $
*
* $Log: cscom.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:50  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSCOM(STR,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)
      CHARACTER *(*)STR
      INTEGER M(10)
#if !defined(CERNLIB_APOLLO)
      N=LEN(STR)
#endif
#if defined(CERNLIB_APOLLO)
      N=80
#endif
      NOPAR=1
      DO 1 I=1,N
      IF(STR(I:I).EQ.'#')GO TO 3
      IF(STR(I:I).EQ.',')NOPAR=NOPAR+1
  1   CONTINUE
  3   N=NOPAR
      M(1)=LOCF(P1)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(2)=LOCF(P2)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(3)=LOCF(P3)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(4)=LOCF(P4)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(5)=LOCF(P5)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(6)=LOCF(P6)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(7)=LOCF(P7)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(8)=LOCF(P8)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(9)=LOCF(P9)
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(10)=LOCF(P10)
      N=N-1
      IF(N.EQ.0)GO TO 2
      WRITE(*,77)
 77   FORMAT(4X,'CSCOM: TOO MANY PARAMETERS')
      NOPAR=10
 2    CALL CSCOMX(STR,NOPAR,M)
      END
