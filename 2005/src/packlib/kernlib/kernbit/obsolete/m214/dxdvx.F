*
* $Id: dxdvx.F,v 1.1.1.1 1996/02/15 17:47:37 mclareni Exp $
*
* $Log: dxdvx.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:37  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE DXDVX(IARRAY,NWORDS)
C
C             This is a subroutine to convert from IBM double precision
C             floating point format (64 Bits) to VAX long floating point
C             format (64 Bits)
C
C             IARRAY   an area of storage 64*NWORDS bits long in which
C                      are stored one next to the other NWORDS 64 bits
C                      IBM floating point numbers. On output it will
C                      contain NWORDS 64 bits VAX floating point numbers
C
C             NWORDS   number of floating point numbers to convert
C
      DIMENSION IARRAY(*),IDUMMY(2)
      LOGICAL BTEST
      DATA IBIG /Z FFFF 7FFF/
      DATA ISMA /Z 0000 0080/
      IF(NWORDS.LE.0) GO TO 999
      DO 2 J=1,2*NWORDS,2
C
C             Check for exact 0
C
         IDUMMY(1) = 0
         IDUMMY(2) = 0
         IF(IARRAY(J).EQ.0) GO TO 1
C
C             Get exponent
C
         IEXPO  = ISHFT(IARRAY(J),1)
         IEXPO  = ISHFT(IEXPO,-25)
         IF(BTEST(IARRAY(J),23)) THEN
            ILEFT = 0
         ELSEIF(BTEST(IARRAY(J),22)) THEN
            ILEFT = 1
         ELSEIF(BTEST(IARRAY(J),21)) THEN
            ILEFT = 2
         ELSEIF(BTEST(IARRAY(J),20)) THEN
            ILEFT = 3
         END IF
         IEXPO = IEXPO * 4 - ILEFT - 128
         IF(IEXPO.LE.0) THEN
            IDUMMY(1) = 0
            IDUMMY(2) = ISMA
            GO TO 1
         ELSEIF(IEXPO.GT.255) THEN
            IDUMMY(1) = NOT(0)
            IDUMMY(2) = IBIG
            GO TO 1
         END IF
         IDUMMY(1) = ISHFT(IARRAY(J+1),ILEFT)
         IDUMMY(2) = IOR(ISHFT(ISHFT(IARRAY(J),9+ILEFT),-9),
     +   ISHFT(IARRAY(J+1),-32+ILEFT))
         IDUMMY(1) = IOR(ISHFT(IDUMMY(1),16),ISHFT(IDUMMY(1),-16))
         IDUMMY(2) = IOR(ISHFT(IDUMMY(2),16),ISHFT(IDUMMY(2),-16))
         IDUMMY(2) = IOR(IDUMMY(2),ISHFT(IEXPO,7))
   1     IF(BTEST(IARRAY(J),31)) IDUMMY(2) = IBSET(IDUMMY(2),15)
         IARRAY(J  ) = IDUMMY(1)
         IARRAY(J+1) = IDUMMY(2)
   2  CONTINUE
 999  END
