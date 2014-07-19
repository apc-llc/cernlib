*
* $Id: cscomc.F,v 1.2 1996/03/14 13:43:50 berezhno Exp $
*
* $Log: cscomc.F,v $
* Revision 1.2  1996/03/14 13:43:50  berezhno
* mods for WINNT
*
* Revision 1.1.1.1  1996/02/26 17:16:54  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :          23/10/95  14.51.10  by  Julian Bunn
*-- Author : V.Berezhnoi
      SUBROUTINE CSCOMC(STR,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)
      CHARACTER *(*)STR,P1,P2
      character*(*) P3,P4,P5,P6,P7,P8,P9,P10
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
#if defined(CERNLIB_VAX)
      M(1)=%LOC(P1)/4
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(2)=%LOC(P2)/4
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(3)=%LOC(P3)/4
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(4)=%LOC(P4)/4
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(5)=%LOC(P5)/4
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(6)=%LOC(P6)/4
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(7)=%LOC(P7)/4
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(8)=%LOC(P8)/4
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(9)=%LOC(P9)/4
      N=N-1
      IF(N.EQ.0)GO TO 2
      M(10)=%LOC(P10)/4
      N=N-1
      IF(N.EQ.0)GO TO 2
#endif
#if !defined(CERNLIB_VAX)
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
#endif
      WRITE(*,77)
 77   FORMAT(4X,'CSCOMC: TOO MANY PARAMETERS')
      NOPAR=10
 2    CALL CSCOMX(STR,NOPAR,M)
      END
