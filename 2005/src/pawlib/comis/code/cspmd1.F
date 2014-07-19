*
* $Id: cspmd1.F,v 1.1.1.1 1996/02/26 17:16:26 mclareni Exp $
*
* $Log: cspmd1.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:26  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/02 09/07/93  15.29.15  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSPMD1(M,MD,P1)
      INTEGER M(10),MD(11)
      M(1)=LOCF(P1)
#if defined(CERNLIB_IBM)
      IF(MD(2).EQ. -7)M(1)=JUMPAD(P1)
#endif
#if !defined(CERNLIB_IBM)
      IF(MD(2).EQ. -7)M(1)=LOCB(P1)
#endif
      END
