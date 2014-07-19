*
* $Id: cfribmsf.F,v 1.1.1.1 1996/03/08 15:21:49 mclareni Exp $
*
* $Log: cfribmsf.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:49  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_STF77)
      SUBROUTINE CFRIBM(IARRAY,NW,MODE)
C      ++++++++ STANDARD FORTRAN VERSION +++++++++++
C              The numbers must be right adj., zero filled.
C              MODE = 1 : convert 16-bit integers
C                   = 2 : convert 32-bit integers (Dummy)
C                   = 3 : convert 32-bit floating
      LOGICAL BTEST
      DIMENSION IARRAY(*)
C     PARAMETER (IBIG=16#7FFF FFFF,ISMA=16#0080 0000)
      PARAMETER (IBIG=2147483647,ISMA=8388608)
C
      IF(NW.LE.0)   GO TO 999
      IF(MODE.EQ.1) THEN
C
C                16-bit integers
C
        DO 110,I=1,NW
          IARRAY(I) = IAND(IARRAY(I),65535)
 110    CONTINUE
      ELSEIF(MODE.EQ.2) THEN
C
C                32-bit integers (no conversion)
C
      ELSE IF(MODE.EQ.3) THEN
C
C                Convert the first NW words of IARRAY from
C                IBM 32-bit floating point representation to
C                IEEE single-precision (32-bit) floating point
C                representation
        DO 2 J=1,NW
C
C                Check for exact 0
           IF(IARRAY(J).EQ.0) GO TO 2
C
C              Make sure ILEFT gets defined to something!
           ILEFT = 0
C
C                Get exponent
           IEXPO  = ISHFT(ISHFT(IARRAY(J),1),-25)
           IF(BTEST(IARRAY(J),23)) THEN
              ILEFT = 0
           ELSEIF(BTEST(IARRAY(J),22)) THEN
              ILEFT = 1
           ELSEIF(BTEST(IARRAY(J),21)) THEN
              ILEFT = 2
           ELSEIF(BTEST(IARRAY(J),20)) THEN
              ILEFT = 3
           END IF
           IEXPO = IEXPO * 4 - ILEFT - 130
           IF(IEXPO.LE.0) THEN
              IDUMMY = ISMA
              GO TO 1
           ELSEIF(IEXPO.GT.255) THEN
              IDUMMY = IBIG
              GO TO 1
           END IF
           IDUMMY = ISHFT(IARRAY(J),ILEFT)
           IDUMMY = IOR(IAND(IDUMMY,2**23-1),ISHFT(IEXPO,23))
   1       IF(BTEST(IARRAY(J),31)) IDUMMY = IBSET(IDUMMY,31)
           IARRAY(J) = IDUMMY
   2    CONTINUE
      END IF
C
 999  END
#endif
