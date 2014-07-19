*
* $Id: dcddx.F,v 1.1.1.1 1996/02/15 17:47:38 mclareni Exp $
*
* $Log: dcddx.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:38  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE DCDDX(IARRAY,NWORDS)
C
C             This is a subroutine to convert from CDC long floating
C             point format (120 Bits) to IBM double precision floating
C             point format (64 Bits)
C
C             IARRAY   an area of storage 128*NWORDS bits long in which
C                      are stored NWORDS 120 bits CDC floating point
C                      numbers right adjusted in pairs of 64 bits areas,
C                      60 bits in each pair, right adjusted with zero
C                      fill. On output it will contain NWORDS 64 bits
C                      IBM double precision floating point numbers one
C                      next to the other.
C
C             NWORDS   number of floating point numbers to convert
C
      DIMENSION IARRAY(*),IDUMMY(2)
      LOGICAL BTEST,SET,CLEAR
      DATA MASK/Z00FFFFFF/,IBIG/Z7FFFFFFF/,ISMALL/Z00100000/
      DATA ICEX/Z000007FF/,NCEX/ZFFFFFC00/
      IF(NWORDS.LE.0) GO TO 999
      DO 2 J=1,NWORDS
C
C             Get sign and exponent
C
         JJ    = 4*J-3
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
            IARRAY(2*J-1)=0
            IARRAY(2*J  )=0
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
            IDUMMY(1)=IBIG
            IDUMMY(2)=NOT(0)
            GO TO 1
         ELSEIF(IEXPO.LT.1) THEN
            IDUMMY(1)=ISMALL
            IDUMMY(2)=0
            GO TO 1
         END IF
         IEX16 = (IEXPO)/4
         ILEFT = 4 - MOD(IEXPO,4)
         IF(ILEFT.EQ.4) THEN
            ILEFT = 0
            IEX16 = IEX16 - 1
         END IF
         IDUMMY(1) = ISHFT(IEX16,24)
         IDUMMY(2) = 0
         CALL BTMOVE(IARRAY(JJ),17,IDUMMY,9+ILEFT,48)
         CALL BTMOVE(IARRAY(JJ),81,IDUMMY,57+ILEFT,8-ILEFT)
         IF(ISIGN.NE.0) THEN
            IMASK = 2**(24-ILEFT)-1
            IDUMMY(1) = IOR(IAND(NOT(IMASK),IDUMMY(1)),
     +                      IAND(IMASK,NOT(IDUMMY(1))))
            IDUMMY(2) = NOT(IDUMMY(2))
         END IF
   1     CALL SBIT(ISIGN,IDUMMY,32)
         IARRAY(2*J-1)=IDUMMY(1)
         IARRAY(2*J  )=IDUMMY(2)
   2  CONTINUE
 999  END
