*
* $Id: blo16stv.F,v 1.1.1.1 1996/03/08 15:21:47 mclareni Exp $
*
* $Log: blo16stv.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:47  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if (defined(CERNLIB_STF77VX))&&(!defined(CERNLIB_STF77))
      SUBROUTINE BLO16W(SOURCE,N1,TARGET,N2,N3)
      INTEGER SOURCE(*),TARGET(*)
C
C  STANDARD FORTRAN Vax version to do BLO16W with NO external calls.
C
C  Input SOURCE array is a CONTIGUOUS bit string N3*16 bits long
C  starting at 16-BIT POSITION N1 in the array SOURCE.
C  This bit string is BLOWN (unpacked) into N3 computer words
C  starting at TARGET(N2). Each word of TARGET then contains
C  16 bits, left justified.
C
C  16-bit word number within Computer word increases
C  as one goes from Least Significant Bit (LSB) to MSB.
C  i.e.
C     63       48 47       32 31      16 15       0
C    -----------------------------------------------
C    |     4     |     3     |     2    |     1    |
C    -----------------------------------------------
C
#include "epio/wordsize.inc"
      IF(N3 .LE. 0) GO TO 999
C
C                  SOURCE word
      IWSRC =  (N1-1)/N16PW
      I16SRC = N1-IWSRC*N16PW
      IWSRC  = IWSRC+1
      NBITR  = (I16SRC-1)*16
C
      DO 10 IWTGT=N2,N2+N3-1
C
C                  Extract 16-bit word from source
      JWD16 = ISHFT(SOURCE(IWSRC),-NBITR)
      TARGET(IWTGT) = IAND(JWD16,65535)
C
C
      IF(NBITR.EQ.NBITPW-16) THEN
         NBITR = 0
         IWSRC = IWSRC + 1
      ELSE
         NBITR = NBITR + 16
      ENDIF
C
  10  CONTINUE
C
 999  END
#endif
