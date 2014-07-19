*
* $Id: ctoibmap.F,v 1.1.1.1 1996/03/08 15:21:50 mclareni Exp $
*
* $Log: ctoibmap.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:50  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_APOLLO)
      SUBROUTINE CTOIBM(IARRAY,NW,MODE)
C              +++++++++++++ APOLLO VERSION ++++++++++++++++++
C              The numbers must be right adj., zero filled.
C              MODE = 1 : convert 16-bit integers
C                   = 2 : convert 32-bit integers (Dummy)
C                   = 3 : convert 32-bit floating
      DIMENSION IARRAY(*)
      INTEGER*2 IW2(2)
      LOGICAL BTEST
      EQUIVALENCE (IW2(1),IW4)
      BTEST(J,N) = RSHFT(LSHFT(J,31-N),31).EQ.1
      IBSET(J,N) = OR(J,LSHFT(1,N))
C
      IF(NW.LE.0)   GO TO 999
      IF(MODE.EQ.1) THEN
C
C                 16-bit integers
C
        IW2(1) = 0
        DO 110,I=1,NW
          IW2(2) = IARRAY(I)
          IARRAY(I) = IW4
 110    CONTINUE
      ELSE IF(MODE.EQ.2) THEN
C
C                 32-bit integers (no conversion)
C
      ELSE IF(MODE.EQ.3) THEN
C
C                Convert the first NW words of IARRAY from APOLLO
C                single-precision (32-bit) floating point representation
C                to IBM 32-bit floating point representation
        DO 10,I=1,NW
C
C             Check for exact 0
          IF(IARRAY(I).EQ.0) GO TO 10
C
C             Get exponent
          IEXPO = RSHFT(LSHFT(IARRAY(I),1),24)
C
C                APOLLO EXP + APOLLO BIAS + IBM BIAS
C         IEXPO = IEXPO   -    128   +   260
          IEXPO = IEXPO + 134
          IEX16 = IEXPO/4
          ILEFT = 4 - MOD(IEXPO,4)
          IF(ILEFT.EQ.4) THEN
             ILEFT = 0
             IEX16 = IEX16 - 1
          END IF
          IDUMMY = AND(IARRAY(I),2**23-1)
          IDUMMY = IBSET(IDUMMY,23)
          IDUMMY = IDUMMY + 2**(ILEFT-1)
          IDUMMY = RSHFT(IDUMMY,ILEFT)
          IDUMMY = OR(IDUMMY,LSHFT(IEX16,24))
          IF(BTEST(IARRAY(I),31)) IDUMMY = IBSET(IDUMMY,31)
          IARRAY(I) = IDUMMY
  10    CONTINUE
      END IF
C
 999  END
#endif
