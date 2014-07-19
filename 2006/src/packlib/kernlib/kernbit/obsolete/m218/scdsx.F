*
* $Id: scdsx.F,v 1.1.1.1 1996/02/15 17:47:37 mclareni Exp $
*
* $Log: scdsx.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:37  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE SCDSX(IARRAY,NWORDS)
C
C             This is a subroutine to convert from CDC short floating
C             point format (60 bits) to IBM short floating point
C             format (32 bits).
C
C             IARRAY   an area of storage 64*NWORDS bits long in which
C                      are stored NWORDS 60 bits CDC floating point
C                      numbers right adjusted in 64 bits doublewords.
C                      On output it will contain NWORDS 32 bits IBM
C                      floating point numbers
C
C             NWORDS   number of floating point numbers to convert
C
      DIMENSION IARRAY(*)
      LOGICAL BTEST,SET,CLEAR
      DATA MASK/Z00FFFFFF/,IBIG/Z7FFFFFFF/,ISMALL/Z00100000/
      DATA ICEX/Z000007FF/,NCEX/ZFFFFFC00/
      IF(NWORDS.LE.0) GO TO 999
      DO 2 J=1,NWORDS
C
C             Get exponent and sign
C
         JJ    = 2*J-1
         IEXPO = 0
         ISIGN = 0
         CALL BTMOVE(IARRAY(JJ),5,ISIGN,32,1)
         CALL BTMOVE(IARRAY(JJ),6,IEXPO,22,11)
C
C             Check for exact 0
C
         SET   =       BTEST(IARRAY(JJ),15).AND.BTEST(ISIGN,0)
     +                                          .AND.BTEST(IEXPO,0)
         CLEAR = .NOT.(BTEST(IARRAY(JJ),15).OR. BTEST(ISIGN,0)
     +                                          .OR. BTEST(IEXPO,0))
         IF(CLEAR.OR.SET) THEN
            IARRAY(J) = 0
            GO TO 2
         END IF
         IF(ISIGN.NE.0) IEXPO = IAND(NOT(IEXPO),ICEX)
         IF(BTEST(IEXPO,10)) THEN
            IEXPO = IBCLR(IEXPO,10)
         ELSE
            IEXPO = IOR(NCEX,IEXPO+1)
         END IF
C                CDC EXP + CDC BIAS + CDC NORM + IBM BIAS
C        IEXPO = IEXPO              +    48    +   260
         IEXPO = IEXPO + 308
         IF(IEXPO.GT.512) THEN
            IDUMMY=IBIG
            GO TO 1
         ELSEIF(IEXPO.LT.1) THEN
            IDUMMY=ISMALL
            GO TO 1
         END IF
         IEX16 = IEXPO/4
         ILEFT = 4 - MOD(IEXPO,4)
         IF(ILEFT.EQ.4) THEN
            ILEFT = 0
            IEX16 = IEX16 - 1
         END IF
         IDUMMY = ISHFT(IEX16,24)
         CALL BTMOVE(IARRAY(JJ),17,IDUMMY,9+ILEFT,24-ILEFT)
         IF(ISIGN.NE.0) THEN
            IMASK = 2**(24-ILEFT)-1
            IDUMMY = IOR(IAND(NOT(IMASK),IDUMMY),
     +                   IAND(IMASK,NOT(IDUMMY)))
         END IF
   1     CALL SBIT(ISIGN,IDUMMY,32)
         IARRAY(J) = IDUMMY
   2  CONTINUE
 999  END
