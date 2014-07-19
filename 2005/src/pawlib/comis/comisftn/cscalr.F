*
* $Id: cscalr.F,v 1.1.1.1 1996/02/26 17:16:53 mclareni Exp $
*
* $Log: cscalr.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:53  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_VAX)||defined(CERNLIB_APOLLO)
*CMZ :  1.14/00 08/02/93  10.28.51  by  Alfred Nathaniel
*-- Author : V.Berezhnoi
      FUNCTION CSCALR(NAME,N,P)
      INTEGER P(15)
      EXTERNAL NAME
      REAL NAME
#if defined(CERNLIB_VAX)
      IF (N.EQ.0)THEN
      CSCALR=NAME()
      ELSEIF(N.GT.15)THEN
      PRINT *,' CS: More then 15 arguments in call to users routine'
      CSCALR=0
      ELSE
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),N
 1    CSCALR=NAME(%VAL(P(1)))
      RETURN
 2    CSCALR=NAME(%VAL(P(1)),%VAL(P(2)))
      RETURN
 3    CSCALR=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)))
      RETURN
 4    CSCALR=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4)))
      RETURN
 5    CSCALR=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     , ,%VAL(P(5)))
      RETURN
 6    CSCALR=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     , ,%VAL(P(5)),%VAL(P(6)))
      RETURN
 7    CSCALR=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     , ,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)))
      RETURN
 8    CSCALR=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,  ,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)))
      RETURN
 9    CSCALR=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     , ,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9)))
      RETURN
 10   CSCALR=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)))
      RETURN
 11   CSCALR=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)),%VAL(P(11)) )
      RETURN
 12   CSCALR=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)),%VAL(P(11)),%VAL(P(12)) )
      RETURN
 13   CSCALR=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)),%VAL(P(11)),%VAL(P(12)),%VAL(P(13)) )
      RETURN
 14   CSCALR=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)),%VAL(P(11)),%VAL(P(12)),%VAL(P(13)),%VAL(P(14)))
      RETURN
 15   CSCALR=NAME(%VAL(P(1)),%VAL(P(2)),%VAL(P(3)),%VAL(P(4))
     ,,%VAL(P(5)),%VAL(P(6)),%VAL(P(7)),%VAL(P(8)),%VAL(P(9))
     ,,%VAL(P(10)),%VAL(P(11)),%VAL(P(12)),%VAL(P(13)),%VAL(P(14))
     ,,%VAL(P(15)))
      RETURN
      ENDIF
#endif
#if defined(CERNLIB_APOLLO)
      IF (N.EQ.0)THEN
      CSCALR=NAME()
      ELSEIF(N.GT.15)THEN
      PRINT *,' CS: More then 15 arguments in call to users routine'
      ELSE
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),N
 1    CSCALR=CSR01F(NAME,P)
      RETURN
 2    CSCALR=CSR02F(NAME,P)
      RETURN
 3    CSCALR=CSR03F(NAME,P)
      RETURN
 4    CSCALR=CSR04F(NAME,P)
      RETURN
 5    CSCALR=CSR05F(NAME,P)
      RETURN
 6    CSCALR=CSR06F(NAME,P)
      RETURN
 7    CSCALR=CSR07F(NAME,P)
      RETURN
 8    CSCALR=CSR08F(NAME,P)
      RETURN
 9    CSCALR=CSR09F(NAME,P)
      RETURN
 10   CSCALR=CSR10F(NAME,P)
      RETURN
 11   CSCALR=CSR11F(NAME,P)
      RETURN
 12   CSCALR=CSR12F(NAME,P)
      RETURN
 13   CSCALR=CSR13F(NAME,P)
      RETURN
 14   CSCALR=CSR14F(NAME,P)
      RETURN
 15   CSCALR=CSR15F(NAME,P)
      ENDIF
#endif
      END
#endif
