*
* $Id: cspmd4.F,v 1.1.1.1 1996/02/26 17:16:26 mclareni Exp $
*
* $Log: cspmd4.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:26  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/02 09/07/93  15.31.47  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSPMD4(M,MD,P1,P2,P3,P4)
      INTEGER M(10),MD(11)
      M(1)=LOCF(P1)
      M(2)=LOCF(P2)
      M(3)=LOCF(P3)
      M(4)=LOCF(P4)
#if defined(CERNLIB_IBM)
      IF(MD(2).EQ. -7)M(1)=JUMPAD(P1)
      IF(MD(3).EQ. -7)M(2)=JUMPAD(P2)
      IF(MD(4).EQ. -7)M(3)=JUMPAD(P3)
      IF(MD(5).EQ. -7)M(4)=JUMPAD(P4)
#endif
#if !defined(CERNLIB_IBM)
      IF(MD(2).EQ. -7)M(1)=LOCB(P1)
      IF(MD(3).EQ. -7)M(2)=LOCB(P2)
      IF(MD(4).EQ. -7)M(3)=LOCB(P3)
      IF(MD(5).EQ. -7)M(4)=LOCB(P4)
#endif
      END
