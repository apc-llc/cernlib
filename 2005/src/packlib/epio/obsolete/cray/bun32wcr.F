*
* $Id: bun32wcr.F,v 1.1.1.1 1996/03/08 15:21:54 mclareni Exp $
*
* $Log: bun32wcr.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:54  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CRAY)
      SUBROUTINE BUN32W(SOURCE,N1,TARGET,N2,N3)
      INTEGER SOURCE(*),TARGET(*)
C
C
C  CRAY version to do BUN32W with NO external calls.
C
C                         N.McCubbin   18-Feb-87
C  Revised                F.Carminati  23-Jan-89
C                         F.Carminati  02-Jun-89
C                         M.J.Corden   30-Jan-91
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
      DIMENSION MBITL(4),NBITL(4),IWSRC(4),IWTGT(4),N3J(4)
      DIMENSION MASSRC(4),MASTGT(4)
      DATA MBITL /48,32,16,0/
 
      DATA MASSRC /X'00000000FFFF0000',X'000000000000FFFF',
     &             X'00000000FFFF0000',X'000000000000FFFF'/
      SAVE MBITL,MASSRC
C
      IF(N3 .LE. 0) GO TO 999
C
      DO 1 JS = 1,4
        N3J(JS)   = (JS-1)/2
        IWSRC(JS) = N1+N3J(JS)
        IWTGT(JS) = (N2+JS+2)/4
        ILR       = 16*MOD(JS,2)
        NBITL(JS) = MBITL(MOD(N2+JS+2,4)+1) - ILR
        N3J(JS)   = (N3+1-N3J(JS))/2 - 1
        MASTGT(JS)= .NOT.(ISHFT(MASSRC(JS),NBITL(JS)))
   1  CONTINUE
C
      DO 2 JS = 1,4
        DO 3 I = 0,N3J(JS)
          IEXT = SOURCE(IWSRC(JS)+2*I) .AND. MASSRC(JS)
          IEXT = ISHFT(IEXT,NBITL(JS))
          TARGET(IWTGT(JS)+I) = TARGET(IWTGT(JS)+I) .AND. MASTGT(JS)
          TARGET(IWTGT(JS)+I) = TARGET(IWTGT(JS)+I) .OR. IEXT
   3    CONTINUE
   2  CONTINUE
C
 999  END
#endif
