*
* $Id: blo16cra.F,v 1.1.1.1 1996/03/08 15:21:54 mclareni Exp $
*
* $Log: blo16cra.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:54  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CRAY)
CDECK,BLO16CRA,IF=CRAY,CONVEX.
      SUBROUTINE BLO16W(SOURCE,N1,TARGET,N2,N3)
      INTEGER SOURCE(*),TARGET(*)
C
C  CRAY version to do BLO16W with NO external calls.
C
C                         N.McCubbin   17-Mar-87
C  Revised                F.Carminati  23-Jun-87
C                         F.Carminati  02-Jun-89
C
C  Input SOURCE array is a CONTIGUOUS bit string N3*16 bits long
C  starting at 16-BIT POSITION N1 in the array SOURCE.
C  This bit string is BLOWN (unpacked) into N3 computer words
C  starting at TARGET(N2). Each word of TARGET then contains
C  16 bits, right justified.
C
C  16-bit word number within Computer word increases
C  as one goes from Most Significant Bit (MSB) to LSB.
C  i.e.
C     63       48 47       32 31      16 15       0
C    -----------------------------------------------
C    |     1     |     2     |     3    |     4    |
C    -----------------------------------------------
C
      IF(N3 .LE. 0) GO TO 999
C
C                  SOURCE word
      IWSRC =  (N1-1)/4
      I16SRC = N1-IWSRC*4
      IWSRC  = IWSRC+1
      NBITR  = (4-I16SRC)*16
C
      DO 10 IWTGT=N2,N2+N3-1
C
C                  Extract 16-bit word from source
      JWD16 = ISHFT(SOURCE(IWSRC),-NBITR)
      TARGET(IWTGT) = IAND(JWD16,65535)
C
C
      IF(NBITR.EQ.0) THEN
         NBITR = 48
         IWSRC = IWSRC + 1
      ELSE
         NBITR = NBITR - 16
      ENDIF
C
  10  CONTINUE
C
 999  END
#endif
