*
* $Id: cfribmap.F,v 1.1.1.1 1996/03/08 15:21:50 mclareni Exp $
*
* $Log: cfribmap.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:50  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_APOLLO)
      SUBROUTINE CFRIBM(IARRAY,NW,MODE)
C              +++++++++++++ APOLLO VERSION ++++++++++++++++++
C              The numbers must be right adj., zero filled.
C              MODE = 1 : convert 16-bit integers
C                   = 2 : convert 32-bit integers (Dummy)
C                   = 3 : convert 32-bit floating
      DIMENSION IARRAY(*)
      LOGICAL BTEST
      INTEGER*2 IW2(2)
      EQUIVALENCE (IW2(1),IW4)
      PARAMETER (IBIG=16#7FFF FFFF,ISMA=16#0080 0000)
      BTEST(J,N) = RSHFT(LSHFT(J,31-N),31).EQ.1
      IBSET(J,N) = OR(J,LSHFT(1,N))
C
      IF(NW.LE.0)   GO TO 999
      IF(MODE.EQ.1) THEN
C
C                16-bit integers
C
        DO 110,I=1,NW
          IW4 = IARRAY(I)
          IARRAY(I) = IW2(2)
 110    CONTINUE
      ELSEIF(MODE.EQ.2) THEN
C
C                32-bit integers (no conversion)
C
      ELSE IF(MODE.EQ.3) THEN
C
C                Convert the first NW words of IARRAY from
C                IBM 32-bit floating point representation to
C                APOLLO single-precision (32-bit) floating point
C                representation
        DO 2 J=1,NW
C
C                Check for exact 0
           IF(IARRAY(J).EQ.0) GO TO 2
C
C                Get exponent
C              Make sure ILEFT gets defined to something!
           ILEFT = 0
C
           IEXPO  = RSHFT(LSHFT(IARRAY(J),1),25)
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
           IDUMMY = LSHFT(IARRAY(J),ILEFT)
           IDUMMY = OR(AND(IDUMMY,2**23-1),LSHFT(IEXPO,23))
   1       IF(BTEST(IARRAY(J),31)) IDUMMY = IBSET(IDUMMY,31)
           IARRAY(J) = IDUMMY
   2    CONTINUE
      END IF
C
 999  END
#endif
