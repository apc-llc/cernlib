*
* $Id: parofi.F,v 1.1.1.1 1996/02/15 17:53:27 mclareni Exp $
*
* $Log: parofi.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:27  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION PAROFI (IN)
C
C-    THIS FLOATING/INTEGER FUNCTION CANNOT BE WRITTEN TO SATISFY
C-    ALL COMPILERS.   PLEASE PATCHY-MODIFY IT AS NEEDED ON YOURS.
C-    WE ASSERT THIS ROUTINE WILL NOT BE CHANGED AGAIN.
C
      DIMENSION    IN(9), MPP(49),MPT1(30),MPT2(11),MPT3(8)
      EQUIVALENCE (MPT1(1),MPP(1)),(MPT2(1),MPP(31)),(MPT3(1),MPP(42))
      DIMENSION    MALT(8), IALT(9)
      EQUIVALENCE (RESULT,IRESUL)
C
      DATA   MPT1 /  1H0,    1HG,    2HNU,   1HE,    2HMU,   3HPI0,
     +               2HPI,   1HK,    2HK0,   1HP,    1HN,    1HL,
     +               2HS+,   2HS0,   2HS-,   3HXI0,  3HXI-,  1HD,
     +               3HTRI,  3HHE3,  3HHE4,  4H(22), 4H(23), 4H(24),
     +               4H(25), 4H(26), 4H(27), 4H(28), 4H(29), 4H(30)/
C
      DATA   MPT2 /  11*4H****  /
C
      DATA   MPT3 /  2HAP,   2HAN,   2HAL,   3HAS+,  3HAS0,  3HAS-,
     +               4HAXI0, 4HAXI-/
C
      DATA   MALT / 4HZERO,1HO,3HPIO,2HKO,2HSO,3HXIO,3HASO,4HAXIO/
      DATA  IALT/ 0,     1,  1,    6,   9,  14,   16,   46,    48/
C
C-----  ENTRY PAROFI
C
      N = IN(1)
      IF (N.LE.0. OR.N.GT.49) N=40
      IRESUL = MPP(N)
      PAROFI = RESULT
      RETURN
C
         ENTRY IOFPAR                                                    CDC
C        ENTRY IOFPAR  (IN)                                             -CDC
C
      N = IN(1)
      IRESUL = IUCOMP (N,MPP(1),49)
      IF (IRESUL.NE.0)       GO TO 39
      IRESUL = IUCOMP (N,MALT(1),8)
      IRESUL = IALT(IRESUL+1)
   39 PAROFI = RESULT                                                    CDC
C  39 IOFPAR = IRESUL                                                   -CDC
      RETURN
      END
