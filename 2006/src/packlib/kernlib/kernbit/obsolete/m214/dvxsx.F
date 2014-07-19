*
* $Id: dvxsx.F,v 1.1.1.1 1996/02/15 17:47:36 mclareni Exp $
*
* $Log: dvxsx.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:36  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE DVXSX(IARRAY,NWORDS)
C
C             This is a subroutine to convert from VAX double precision
C             floating point format (64 Bits) to IBM floating format
C             (32 bits).
C
C             IARRAY   an area of storage 64*NWORDS bits long in which
C                      are stored NWORDS 64 bits VAX floating point
C                      numbers.
C                      On output it will contain NWORDS 32 bits IBM
C                      floating point numbers
C
C             NWORDS   number of floating point numbers to convert
C
      DIMENSION IARRAY(*)
      LOGICAL BTEST
      IF(NWORDS.LE.0) GO TO 999
      DO 1 J=2,2*NWORDS,2
C
C             Check for exact 0
C
         IF(IARRAY(J).EQ.0) GO TO 1
C
C             Get exponent
C
         IEXPO = ISHFT(ISHFT(IARRAY(J),17),-24)
C                VAX EXP + VAX BIAS + IBM BIAS
C        IEXPO = IEXPO   -    128   +   260
         IEXPO = IEXPO + 132
         IEX16 = IEXPO/4
         ILEFT = 4 - MOD(IEXPO,4)
         IF(ILEFT.EQ.4) THEN
            ILEFT = 0
            IEX16 = IEX16 - 1
         END IF
         IDUMMY = ISHFT(ISHFT(IARRAY(J),25),-9)
         IDUMMY = IBSET(IDUMMY,23)
         IDUMMY = IOR(IDUMMY,ISHFT(IARRAY(J),-16))
         IDUMMY = IDUMMY + 2 ** (ILEFT-1)
         IDUMMY = ISHFT(IDUMMY,-ILEFT)
         IF(BTEST(IDUMMY,24)) THEN
            IDUMMY = IDUMMY + 1
            IDUMMY = ISHFT(IDUMMY,-4)
            IEX16  = IEX16 + 1
         END IF
         IDUMMY = IOR(IDUMMY,ISHFT(IEX16,24))
         IF(BTEST(IARRAY(J),15)) IDUMMY = IBSET(IDUMMY,31)
         IARRAY(J/2) = IDUMMY
   1  CONTINUE
 999  END
