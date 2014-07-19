*
* $Id: ccopysa.F,v 1.1.1.1 1996/02/26 17:16:59 mclareni Exp $
*
* $Log: ccopysa.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:59  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_ALPHA_OSF)
*CMZ :  1.16/13 16/09/93  16.08.22  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CCOPYS(J1,J2,N)
      IF(N.LT.1)RETURN
      IF(J2-J1) 2,4,1
    1 IF(J2-(J1+N)) 5,2,2
    2 DO 3 LOOP=1,N
         CALL CSCHAR(J2+LOOP-1,MKCHAR(J1+LOOP-1))
    3 CONTINUE
    4 RETURN
C---SPECIAL CASE
    5 J1MOD=J1+N
      J2MOD=J2+N
      DO 6 LOOP=1,N
         CALL CSCHAR(J2MOD-LOOP,MKCHAR(J1MOD-LOOP))
    6 CONTINUE
      RETURN
      END
#endif
