*
* $Id: blo8stv.F,v 1.1.1.1 1996/03/08 15:21:48 mclareni Exp $
*
* $Log: blo8stv.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:48  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if (defined(CERNLIB_STF77VX))&&(!defined(CERNLIB_STF77))
      SUBROUTINE BLO8W(SOURCE,N1,TARGET,N2,N3)
      INTEGER SOURCE(*),TARGET(*)
C
C  STANDARD FORTRAN Vax version to do BLO8W with NO external calls.
C
C  Input SOURCE array is a CONTIGUOUS bit string N3*8 bits long
C  starting at 8-BIT POSITION N1 in the array SOURCE.
C  This bit string is BLOWN (unpacked) into N3 computer words
C  starting at TARGET(N2). Each word of TARGET then contains
C  8 bits, left justified.
C
C  8-bit word number within Computer word increases
C  as one goes from Least Significant Bit (LSB) to MSB.
C  i.e.
C     63       48 47       32 31      16 15       0
C    -----------------------------------------------
C    |     4     |     3     |     2    |     1    |
C    -----------------------------------------------
C
#include "epio/wordsize.inc"
      IF(N3 .LE. 0) GO TO 999
      N8PW = N16PW * 2
C
C                  SOURCE word
      IWSRC =  (N1-1)/N8PW
      I8SRC = N1-IWSRC*N8PW
      IWSRC  = IWSRC+1
      NBITR  = (I8SRC-1)*8
C
      DO 10 IWTGT=N2,N2+N3-1
C
C                  Extract 8-bit word from source
      JWD8 = ISHFT(SOURCE(IWSRC),-NBITR)
      TARGET(IWTGT) = IAND(JWD8,255)
C
C
      IF(NBITR.EQ.NBITPW-8) THEN
         NBITR = 0
         IWSRC = IWSRC + 1
      ELSE
         NBITR = NBITR + 8
      ENDIF
C
  10  CONTINUE
C
 999  END
#endif
