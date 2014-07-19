*
* $Id: ctoibstv.F,v 1.1.1.1 1996/03/08 15:21:49 mclareni Exp $
*
* $Log: ctoibstv.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:49  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_STF77VX)
      SUBROUTINE CTOIBM(IARRAY,NW,MODE)
C Aleph version
C      +++++++ STANDARD FORTRAN VERSION +++++++
C              The numbers must be right adj., zero filled.
C              MODE = 1 : convert 16-bit integers
C                   = 2 : convert 32-bit integers (Dummy)
C                   = 3 : convert 32-bit floating
      LOGICAL BTEST
      DIMENSION IARRAY(*)
CLatB
      integer*2 sw0(2), sw1(2)
      integer lw0, lw1
      equivalence (sw0,lw0)
      equivalence (sw1,lw1)
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
C                 32-bit integers (word swap)
C
         do 120 i=1, nw
             lw0 = iarray(i)
             sw1(2) = sw0(1)
             sw1(1) = sw0(2)
             iarray(i) = lw1
 120     continue
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
         do 130 i=1, nw
             lw0 = iarray(i)
             sw1(2) = sw0(1)
             sw1(1) = sw0(2)
             iarray(i) = lw1
 130     continue
      END IF
C
 999  continue
      END
#endif
