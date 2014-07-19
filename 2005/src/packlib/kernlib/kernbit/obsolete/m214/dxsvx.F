*
* $Id: dxsvx.F,v 1.1.1.1 1996/02/15 17:47:37 mclareni Exp $
*
* $Log: dxsvx.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:37  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE DXSVX(IARRAY,NWORDS)
C
C             This is a subroutine to convert from IBM double precision
C             floating point format (64 Bits) to VAX short floating
C             point format (32 Bits)
C
C             IARRAY   an area of storage 64*NWORDS bits long in which
C                      are stored one next to the other NWORDS 32 bits
C                      IBM floating point numbers. On output it will
C                      contain NWORDS 32 bits VAX floating point numbers
C                      each stored in a 64 bits area, right adjusted with
C                      zero fill
C
C             NWORDS   number of floating point numbers to convert
C
      DIMENSION IARRAY(*)
      LOGICAL BTEST
      DATA IBIG /Z FFFF 7FFF/
      DATA ISMA /Z 0000 0080/
      IF(NWORDS.LE.0) GO TO 999
      DO 2 J=1,2*NWORDS,2
C
C             Check for exact 0
C
         IF(IARRAY(J).EQ.0) GO TO 2
C
C             Get exponent and sign
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
         IDUMMY = ISHFT(IAND(IARRAY(J),2**24-1),ILEFT+1)
         IDUMMY = IOR(IDUMMY,ISHFT(IARRAY(J+1),-31+ILEFT))+1
         ISPILL = ISHFT(IDUMMY,-25)
         IDUMMY = ISHFT(IDUMMY,-ISPILL-1)
         IEXPO  = IEXPO+ISPILL
         IF(IEXPO.LE.0) THEN
            IDUMMY = ISMA
            GO TO 1
         ELSEIF(IEXPO.GT.255) THEN
            IDUMMY = IBIG
            GO TO 1
         END IF
         IDUMMY = IOR(ISHFT(IDUMMY,16),ISHFT(ISHFT(IDUMMY,9),-25))
         IDUMMY = IOR(IDUMMY,ISHFT(IEXPO,7))
   1     IF(BTEST(IARRAY(J),31)) IDUMMY = IBSET(IDUMMY,15)
         IARRAY(J+1) = IDUMMY
         IARRAY(J  ) = 0
   2  CONTINUE
 999  END
