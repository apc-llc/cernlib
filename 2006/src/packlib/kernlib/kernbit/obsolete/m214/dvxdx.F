*
* $Id: dvxdx.F,v 1.1.1.1 1996/02/15 17:47:36 mclareni Exp $
*
* $Log: dvxdx.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:36  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE DVXDX(IARRAY,NWORDS)
C
C             This is a subroutine to convert from VAX floating
C             point format (64 Bits) to IBM double precision floating
C             format (64 bits).
C
C             IARRAY   an area of storage 64*NWORDS bits long in which
C                      are stored NWORDS 64 bits VAX floating point
C                      numbers.
C                      On output it will contain NWORDS 64 bits IBM
C                      floating point numbers
C
C             NWORDS   number of floating point numbers to convert
C
      LOGICAL BTEST
      DIMENSION IARRAY(*),IDUMMY(2)
      IF(NWORDS.LE.0) GO TO 999
      DO 2 J=2,2*NWORDS,2
C
C             Check for exact 0
C
         IF(IARRAY(J).NE.0) GO TO 1
            IARRAY(2*J-1) = 0
            IARRAY(2*J  ) = 0
            GO TO 2
C
C             Get exponent
C
    1    IEXPO = ISHFT(ISHFT(IARRAY(J),17),-24)
C                VAX EXP + VAX BIAS + IBM BIAS
C        IEXPO = IEXPO   -    128   +   260
         IEXPO = IEXPO + 132
         IEX16 = IEXPO/4
         ILEFT = 4 - MOD(IEXPO,4)
         IF(ILEFT.EQ.4) THEN
            ILEFT = 0
            IEX16 = IEX16 - 1
         END IF
         IDUMMY(1) = ISHFT(ISHFT(IARRAY(J),25),-9)
         IDUMMY(1) = IBSET(IDUMMY(1),23)
         IDUMMY(1) = IOR(IDUMMY(1),ISHFT(IARRAY(J),-16))
         ITEST1    = ISHFT(IARRAY(J-1),-16)
         ITEST2    = IAND(IARRAY(J-1),65535)
         ITEST1    = ITEST1 + 2 ** (ILEFT-1)
         ITEST2    = ITEST2 + ISHFT(ITEST1,-16)
         IDUMMY(1) = IDUMMY(1) + ISHFT(ITEST2,-16)
         IDUMMY(2) = IOR(ISHFT(ISHFT(ITEST1,16),-16),ISHFT(ITEST2,16))
         IMOVE     = ILEFT + 4*ISHFT(IDUMMY(1),-24)
         IDUMMY(2) =
     +   IOR(ISHFT(IDUMMY(2),-IMOVE),ISHFT(IDUMMY(1),32-IMOVE))
         IDUMMY(1) = ISHFT(IDUMMY(1),-IMOVE)
         IEX16     = IEX16 + ISHFT(IDUMMY(1),-24)
         IDUMMY(1) = IOR(IDUMMY(1),ISHFT(IEX16,24))
         IF(BTEST(IARRAY(J),15)) IDUMMY(1) = IBSET(IDUMMY(1),31)
         IARRAY(J-1) = IDUMMY(1)
         IARRAY(J  ) = IDUMMY(2)
   2  CONTINUE
 999  END
