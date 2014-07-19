*
* $Id: blo32wcr.F,v 1.1.1.1 1996/03/08 15:21:54 mclareni Exp $
*
* $Log: blo32wcr.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:54  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CRAY)
      SUBROUTINE BLO32W(SOURCE,N1,TARGET,N2,N3)
C
C  CRAY version to do BLO32W with NO external calls.
C
C                         N.McCubbin   17-Mar-87
C  Revised                F.Carminati  23-Jun-87
C                         F.Carminati  02-Jun-89
C                         M.J.Corden   30-Jan-91
C                         H. Grote     04-Apr-91
C
C  Input SOURCE array is a CONTIGUOUS bit string N3*32 bits long
C  starting at 16-BIT POSITION N1 in the array SOURCE.
C  This bit string is BLOWN (unpacked) into N3 computer words
C  starting at TARGET(N2). Each word of TARGET then contains
C  32 bits, right justified.
C
C  16-bit word number within Computer word increases
C  as one goes from Most Significant Bit (MSB) to LSB.
C  i.e.
C     63       48 47       32 31      16 15       0
C    -----------------------------------------------
C    |     1     |     2     |     3    |     4    |
C    -----------------------------------------------
C
      INTEGER SOURCE(1),TARGET(1)
      DIMENSION MBIT(4),MASTGT(2)
      DATA MBIT /16,32,48,0/
C  masks for the two halves of the target word
      DATA MASTGT /X'00000000FFFF0000',X'000000000000FFFF'/
      SAVE MBIT,MASTGT
C
      IF(N3 .LE. 0) GO TO 999
C
C  there are 4 possible byte locations in a source word
C  IWSRC+I is source word #,  NBIT is distance to shift bits
C
      IWSRC = (N1+3)/4
      NBIT = MBIT(MOD(N1+3,4)+1)+16
      DO 1 I=0,(N3-1)/2
        TARGET(N2+2*I) = SHIFT(SOURCE(IWSRC+I),NBIT) .AND. MASTGT(1)
   1  CONTINUE
C
      IWSRC = (N1+4)/4
      NBIT = MBIT(MOD(N1  ,4)+1)
      DO 2 I=0,(N3-1)/2
   2    TARGET(N2+2*I) = TARGET(N2+2*I) .OR.
     &                 (SHIFT(SOURCE(IWSRC+I),NBIT) .AND. MASTGT(2))
C
      IWSRC = (N1+1)/4
      NBIT = MBIT(MOD(N1+1,4)+1)+16
      DO 3 I=1,N3/2
   3    TARGET(N2-1+2*I) = SHIFT(SOURCE(IWSRC+I),NBIT) .AND. MASTGT(1)
C
      IWSRC = (N1+2)/4
      NBIT = MBIT(MOD(N1+2,4)+1)
      DO 4 I=1,N3/2
   4    TARGET(N2-1+2*I) = TARGET(N2-1+2*I) .OR.
     &                   (SHIFT(SOURCE(IWSRC+I),NBIT) .AND. MASTGT(2))
C
 999  END
#endif
