*
* $Id: bun8stf.F,v 1.1.1.1 1996/03/08 15:21:48 mclareni Exp $
*
* $Log: bun8stf.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:48  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if (defined(CERNLIB_STF77))&&(!defined(CERNLIB_STF77VX))
      SUBROUTINE BUN8W(SOURCE,N1,TARGET,N2,N3)
      INTEGER SOURCE(*),TARGET(*)
C
C  STF77 version to do BUN8W with NO external calls.
C
C  Input SOURCE array is N3 computer words starting in
C  SOURCE(N1). The rightmost 8 bits of these N3 words
C  are BUNCHED (packed) into a CONTIGUOUS bit string
C  starting at 8-BIT POSITION N2 in the array TARGET.
C
C  8-bit word number within Computer word increases
C  as one goes from Most Significant Bit (MSB) to LSB.
C  i.e.
C     63       48 47       32 31     16 15       0
C    -----------------------------------------------
C    |     1     |     2     |     3    |     4    |
C    -----------------------------------------------
C
#include "epio/wordsize.inc"
C
      IF(N3 .LE. 0) GO TO 999
C
      N8PW = N16PW*2
C                 And same for TARGET:
      IWTGT  = (N2-1)/N8PW
      I8TGT = N2-IWTGT*N8PW
      IWTGT  = IWTGT+1
      NBITL  = (N8PW-I8TGT)*8
C
      DO 10 IWSRC=N1,N1+N3-1
C
C                 Extract 8-bit word from source
      JWD8 = IAND(SOURCE(IWSRC),255)
C
C                 Shift to correct position for insertion into TARGET
      JWD8 = ISHFT(JWD8,NBITL)
C
C                 Clear TARGET position
      TARGET(IWTGT) = IAND(TARGET(IWTGT),NOT(ISHFT(255,NBITL)))
C
C                 Insert:
      TARGET(IWTGT) = IOR(TARGET(IWTGT),JWD8)
C
      IF(NBITL.EQ.0) THEN
         NBITL = NBITPW - 8
         IWTGT = IWTGT + 1
      ELSE
         NBITL = NBITL - 8
      ENDIF
C
 10   CONTINUE
C
 999  END
#endif
