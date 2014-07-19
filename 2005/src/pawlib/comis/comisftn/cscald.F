*
* $Id: cscald.F,v 1.1.1.1 1996/02/26 17:16:54 mclareni Exp $
*
* $Log: cscald.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:54  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_VAX)||defined(CERNLIB_APOLLO)
*CMZ :  1.12/05 20/03/92  16.04.19  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION CSCALD(NAME,N,P)
      DOUBLE PRECISION CSCALD,NAME
      INTEGER P(15)
      EXTERNAL NAME
#if defined(CERNLIB_VAX)
      IF (N.EQ.0)THEN
      CSCALD=NAME()
      ELSEIF(N.GT.15)THEN
      PRINT *,' CS: More then 15 arguments in call to users routine'
      ELSE
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),N
 1    CSCALD=NAME(%VAL(P(1)))
      RETURN
 2    CSCALD=NAME(%VAL(P(1)),%VAL(P(2)))
      RETURN
 3    CSCALD=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)))
      RETURN
 4    CSCALD=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4)))
      RETURN
 5    CSCALD=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     , ,%VAL(P(5)))
      RETURN
 6    CSCALD=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     , ,%VAL(P(5)),%VAL(P(6)))
      RETURN
 7    CSCALD=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     , ,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)))
      RETURN
 8    CSCALD=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,  ,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)))
      RETURN
 9    CSCALD=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     , ,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9)))
      RETURN
 10   CSCALD=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)))
      RETURN
 11   CSCALD=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)),%VAL(P(11)) )
      RETURN
 12   CSCALD=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)),%VAL(P(11)),%VAL(P(12)) )
      RETURN
 13   CSCALD=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)),%VAL(P(11)),%VAL(P(12)),%VAL(P(13)) )
      RETURN
 14   CSCALD=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)),%VAL(P(11)),%VAL(P(12)),%VAL(P(13)),%VAL(P(14)))
      RETURN
 15   CSCALD=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)),%VAL(P(11)),%VAL(P(12)),%VAL(P(13)),%VAL(P(14))
     ,,%VAL(P(15)))
      RETURN
      ENDIF
#endif
#if defined(CERNLIB_APOLLO)
      DOUBLE PRECISION
     +        CSD01F,CSD02F,CSD03F,CSD04F,CSD05F,CSD06F,CSD07F,
     +        CSD08F,CSD09F,CSD10F,CSD11F,CSD12F,CSD13F,CSD14F,
     +        CSD15F
      IF (N.EQ.0)THEN
      CSCALD=NAME()
      ELSEIF(N.GT.15)THEN
      PRINT *,' CS: More then 15 arguments in call to users routine'
      ELSE
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),N
 1    CSCALD=CSD01F(NAME,P)
      RETURN
 2    CSCALD=CSD02F(NAME,P)
      RETURN
 3    CSCALD=CSD03F(NAME,P)
      RETURN
 4    CSCALD=CSD04F(NAME,P)
      RETURN
 5    CSCALD=CSD05F(NAME,P)
      RETURN
 6    CSCALD=CSD06F(NAME,P)
      RETURN
 7    CSCALD=CSD07F(NAME,P)
      RETURN
 8    CSCALD=CSD08F(NAME,P)
      RETURN
 9    CSCALD=CSD09F(NAME,P)
      RETURN
 10   CSCALD=CSD10F(NAME,P)
      RETURN
 11   CSCALD=CSD11F(NAME,P)
      RETURN
 12   CSCALD=CSD12F(NAME,P)
      RETURN
 13   CSCALD=CSD13F(NAME,P)
      RETURN
 14   CSCALD=CSD14F(NAME,P)
      RETURN
 15   CSCALD=CSD15F(NAME,P)
      ENDIF
#endif
      END
#endif
