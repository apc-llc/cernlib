*
* $Id: bun32wsf.F,v 1.1.1.1 1996/03/08 15:21:48 mclareni Exp $
*
* $Log: bun32wsf.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:48  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if (defined(CERNLIB_STF77))&&(!defined(CERNLIB_STF77VX))
      SUBROUTINE BUN32W(SOURCE,N1,TARGET,N2,N3)
      INTEGER SOURCE(*),TARGET(*)
C
C  STF77 version to do BUN32W with NO external calls.
C
C  Input SOURCE array is N3 computer words starting in
C  SOURCE(N1). The rightmost 32 bits of these N3 words
C  are BUNCHED (packed) into a CONTIGUOUS bit string
C  starting at 16-BIT POSITION N2 in the array TARGET.
C
C  16-bit word number within Computer word increases
C  as one goes from Most Significant Bit (MSB) to LSB.
C  i.e.
C     63       48 47       32 31      16 15       0
C    -----------------------------------------------
C    |     1     |     2     |     3    |     4    |
C    -----------------------------------------------
C
#include "epio/wordsize.inc"
C
      IF(N3 .LE. 0) GO TO 999
C
C                 TARGET word and 16-bit within word
      IWTGT  = (N2-1)/N16PW
      I16TGT = N2-IWTGT*N16PW
      IWTGT  = IWTGT+1
      NBITL  = (N16PW-I16TGT)*16
C
      DO 10 IWSRC=N1,N1+N3-1
C
C                 Extract 16-bit word from source
      JWD16 = IAND(SOURCE(IWSRC),ISHFT(65535,16))
C
C                 Shift to correct position for insertion into TARGET
C
      JWD16 = ISHFT(JWD16,NBITL-16)
C                 Clear TARGET position
      TARGET(IWTGT) = IAND(TARGET(IWTGT),NOT(ISHFT(65535,NBITL)))
C
C                 Insert:
      TARGET(IWTGT) = IOR(TARGET(IWTGT),JWD16)
C
      IF(NBITL.EQ.0) THEN
         NBITL = NBITPW - 16
         IWTGT = IWTGT + 1
      ELSE
         NBITL = NBITL - 16
      END IF
C
C                 Extract 16-bit word from source
      JWD16 = IAND(SOURCE(IWSRC),65535)
C
C                 Shift to correct position for insertion into TARGET
      JWD16 = ISHFT(JWD16, NBITL)
C
C                 Clear TARGET position
      TARGET(IWTGT) = IAND(TARGET(IWTGT),NOT(ISHFT(65535,NBITL)))
C
C                 Insert:
      TARGET(IWTGT) = IOR(TARGET(IWTGT),JWD16)
C
      IF(NBITL.EQ.0) THEN
         NBITL = NBITPW - 16
         IWTGT = IWTGT + 1
      ELSE
         NBITL = NBITL - 16
      END IF
C
 10   CONTINUE
 999  END
#endif
