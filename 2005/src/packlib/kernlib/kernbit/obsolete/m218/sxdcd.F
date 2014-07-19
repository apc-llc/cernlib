*
* $Id: sxdcd.F,v 1.1.1.1 1996/02/15 17:47:38 mclareni Exp $
*
* $Log: sxdcd.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:38  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE SXDCD(IARRAY,NWORDS)
C
C             This is a subroutine to convert from IBM short floating
C             point format (32 Bits) to CDC long floating point format
C             (120 Bits)
C
C             IARRAY   an area of storage 128*NWORDS bits long in which
C                      are stored one next to the other NWORDS 32 bits
C                      IBM floating point numbers. On output it will
C                      contain NWORDS 120 bits CDC floating point numbers
C                      stored in pair of 64 bits areas, 60 bits per area,
C                      right adjusted in with zero fill.
C
C             NWORDS   number of floating point numbers to convert
C
      DIMENSION IARRAY(*)
      LOGICAL BTEST
      DOUBLE PRECISION ZERO(2),ONE(2)
      PARAMETER (IBYTE = 120)
      DATA       ZERO  / Z0000 0000 0000 0000,
     +                   Z0000 0000 0000 0000/,
     +           ONE   / ZFFFF FFFF FFFF FFFF,
     +                   ZFFFF FFFF FFFF FFFF/
      IF(NWORDS.LE.0) GO TO 999
      DO 2 J=NWORDS,1,-1
C
C             Get exponent and sign
C
         JJ     = 4*J-3
         IDUMMY = IARRAY(J)
         ISIGN  = ISHFT(IDUMMY,-31)
         IEXPO  = ISHFT(IDUMMY,1)
         IEXPO  = ISHFT(IEXPO,-25)
         IF(BTEST(IDUMMY,23)) THEN
            ILEFT = 0
         ELSEIF(BTEST(IDUMMY,22)) THEN
            ILEFT = 1
         ELSEIF(BTEST(IDUMMY,21)) THEN
            ILEFT = 2
         ELSEIF(BTEST(IDUMMY,20)) THEN
            ILEFT = 3
         END IF
         IEXPO = IEXPO * 4 - ILEFT - 48 - 256
         IEXP1 = IEXPO - 48
         IF(IEXPO.GE.0) THEN
            IEXPO = IBSET(IEXPO,10)
         ELSE
            IEXPO = IBCLR(IEXPO-1,10)
         END IF
         IF(IEXP1.GE.0) THEN
            IEXP1 = IBSET(IEXP1,10)
         ELSE
            IEXP1 = IBCLR(IEXP1-1,10)
         END IF
         CALL VZERO(IARRAY(JJ),4)
         IF(ISIGN.EQ.0) THEN
            CALL BTMOVE(IEXPO,22,IARRAY(JJ),6,11)
            CALL BTMOVE(IDUMMY,9+ILEFT,IARRAY(JJ),17,24-ILEFT)
            CALL BTMOVE(IEXP1,22,IARRAY(JJ),70,11)
         ELSE
            CALL BTMOVE(ONE ,1,IARRAY(JJ),5,60)
            CALL BTMOVE(ONE ,1,IARRAY(JJ),69,60)
            CALL BTMOVE(NOT(IEXPO),22,IARRAY(JJ),6,11)
            CALL BTMOVE(NOT(IDUMMY),9+ILEFT,IARRAY(JJ),17,24-ILEFT)
            CALL BTMOVE(NOT(IEXP1),22,IARRAY(JJ),70,11)
         END IF
   2  CONTINUE
 999  END
