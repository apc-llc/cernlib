*
* $Id: ctoibshf.F,v 1.1.1.1 1996/03/08 15:21:53 mclareni Exp $
*
* $Log: ctoibshf.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:53  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if (defined(CERNLIB_STF77VX))&&(defined(CERNLIB_NEVER))
      SUBROUTINE CTOIBM(IARRAY,NW,MODE)
C      +++++++ STANDARD FORTRAN VAX VERSION +++++++
C              The numbers must be right adj., zero filled.
C              MODE = 1 : convert 16-bit integers
C                   = 2 : convert 32-bit integers
C                   = 3 : convert 32-bit floating
      LOGICAL BTEST
      DIMENSION IARRAY(*)
C
      IF(NW.LE.0)   GO TO 999
      IF(MODE.EQ.1) THEN
C
C                 16-bit integers
C
        DO 110,I=1,NW
          IARRAY(I) = IAND(IARRAY(I),65535)
 110    CONTINUE
      ELSE IF(MODE.EQ.2) THEN
C
C             Swap 16-bit integers
        DO 120,I=1,NW
          IARRAY(I) = ISHFTC(IARRAY(I),16,32)
 120    CONTINUE
C
      ELSE IF(MODE.EQ.3) THEN
C
C                Convert the first NW words of IARRAY from IEEE
C                single-precision (32-bit) floating point representation
C                to IBM 32-bit floating point representation
        DO 10,I=1,NW
C
C             Check for exact 0
          IF(IARRAY(I).EQ.0) GO TO 10
C
C             Get exponent
          IEXPO = ISHFT(ISHFT(IARRAY(I),1),-24)
C
C                IEEE EXP + IEEE BIAS + IBM BIAS
C         IEXPO = IEXPO   -    128   +   260
          IEXPO = IEXPO + 134
          IEX16 = IEXPO/4
          ILEFT = 4 - MOD(IEXPO,4)
          IF(ILEFT.EQ.4) THEN
             ILEFT = 0
             IEX16 = IEX16 - 1
          END IF
          IDUMMY = IAND(IARRAY(I),2**23-1)
          IDUMMY = IBSET(IDUMMY,23)
          IDUMMY = IDUMMY + 2**(ILEFT-1)
          IDUMMY = ISHFT(IDUMMY,-ILEFT)
          IDUMMY = IOR(IDUMMY,ISHFT(IEX16,24))
          IF(BTEST(IARRAY(I),31)) IDUMMY = IBSET(IDUMMY,31)
          IARRAY(I) = IDUMMY
  10    CONTINUE
      END IF
C
 999  END
#endif
