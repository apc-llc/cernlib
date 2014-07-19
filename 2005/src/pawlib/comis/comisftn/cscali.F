*
* $Id: cscali.F,v 1.1.1.1 1996/02/26 17:16:52 mclareni Exp $
*
* $Log: cscali.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:52  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_VAX)||defined(CERNLIB_APOLLO)
*CMZ :  1.12/05 20/03/92  16.04.19  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSCALI(NAME,N,P)
      INTEGER P(15)
      EXTERNAL NAME
#if defined(CERNLIB_VAX)
      IF (N.EQ.0)THEN
      CSCALI=NAME()
      ELSEIF(N.GT.15)THEN
      PRINT *,' CS: More then 15 arguments in call to users routine'
      ELSE
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),N
 1    CSCALI=NAME(%VAL(P(1)))
      RETURN
 2    CSCALI=NAME(%VAL(P(1)),%VAL(P(2)))
      RETURN
 3    CSCALI=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)))
      RETURN
 4    CSCALI=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4)))
      RETURN
 5    CSCALI=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     , ,%VAL(P(5)))
      RETURN
 6    CSCALI=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     , ,%VAL(P(5)),%VAL(P(6)))
      RETURN
 7    CSCALI=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     , ,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)))
      RETURN
 8    CSCALI=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,  ,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)))
      RETURN
 9    CSCALI=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     , ,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9)))
      RETURN
 10   CSCALI=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)))
      RETURN
 11   CSCALI=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)),%VAL(P(11)) )
      RETURN
 12   CSCALI=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)),%VAL(P(11)),%VAL(P(12)) )
      RETURN
 13   CSCALI=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)),%VAL(P(11)),%VAL(P(12)),%VAL(P(13)) )
      RETURN
 14   CSCALI=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)),%VAL(P(11)),%VAL(P(12)),%VAL(P(13)),%VAL(P(14)))
      RETURN
 15   CSCALI=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)),%VAL(P(11)),%VAL(P(12)),%VAL(P(13)),%VAL(P(14))
     ,,%VAL(P(15)))
      RETURN
      ENDIF
#endif
#if defined(CERNLIB_APOLLO)
      INTEGER CSI01F,CSI02F,CSI03F,CSI04F,CSI05F,CSI06F,CSI07F,
     +        CSI08F,CSI09F,CSI10F,CSI11F,CSI12F,CSI13F,CSI14F,
     +        CSI15F
      IF (N.EQ.0)THEN
      CSCALI=NAME()
      ELSEIF(N.GT.15)THEN
      PRINT *,' CS: More then 15 arguments in call to users routine'
      ELSE
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),N
 1    CSCALI=CSI01F(NAME,P)
      RETURN
 2    CSCALI=CSI02F(NAME,P)
      RETURN
 3    CSCALI=CSI03F(NAME,P)
      RETURN
 4    CSCALI=CSI04F(NAME,P)
      RETURN
 5    CSCALI=CSI05F(NAME,P)
      RETURN
 6    CSCALI=CSI06F(NAME,P)
      RETURN
 7    CSCALI=CSI07F(NAME,P)
      RETURN
 8    CSCALI=CSI08F(NAME,P)
      RETURN
 9    CSCALI=CSI09F(NAME,P)
      RETURN
 10   CSCALI=CSI10F(NAME,P)
      RETURN
 11   CSCALI=CSI11F(NAME,P)
      RETURN
 12   CSCALI=CSI12F(NAME,P)
      RETURN
 13   CSCALI=CSI13F(NAME,P)
      RETURN
 14   CSCALI=CSI14F(NAME,P)
      RETURN
 15   CSCALI=CSI15F(NAME,P)
      ENDIF
#endif
      END
#endif
