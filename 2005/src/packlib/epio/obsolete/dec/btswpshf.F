*
* $Id: btswpshf.F,v 1.1.1.1 1996/03/08 15:21:53 mclareni Exp $
*
* $Log: btswpshf.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:53  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if (!defined(CERNLIB_STF77))&&(defined(CERNLIB_STF77VX))&&(defined(CERNLIB_NEVER))
      SUBROUTINE BTSWAP(SOURCE,NBYTES)
      INTEGER SOURCE(*)
C
C  STF77 VAX version to do BTSWAP with NO external calls.
C
C. SOURCE     SOURCE ARRAY
C. NBYTES     Number of bytes to swap
C
C  16-bit word number within Computer word increases
C  as one goes from Most Significant Bit (MSB) to LSB.
C  i.e.
C     63       48 47       32 31      16 15       0
C    -----------------------------------------------
C    |     1     |     2     |     3    |     4    |
C    -----------------------------------------------
C
*    # of bits/word
#endif
      PARAMETER   (NBITPW=32, NCHAPW=4, NBITPC=8, N16PW=2)
C
      IF(NBYTES .LE. 1) GO TO 999
C
C
C                  SOURCE word
      IWSRC =  1
      NBITR =  0
C
C
      DO 10 IW16=1,NBYTES/2
C
C          Shift bytes and then 16 bit word
      SOURCE(IWSRC) = ISHFTC(ISHFTC(SOURCE(IWSRC),8,16),-16,NBITPW)
C                 Update indices
      IF(NBITR.EQ.NBITPW-16) THEN
         NBITR = 0
         IWSRC = IWSRC + 1
      ELSE
         NBITR = NBITR + 16
      ENDIF
C
C
 10   CONTINUE
C          Shift last word if necessary
      SOURCE(IWSRC) = ISHFTC(SOURCE(IWSRC),NBITR,NBITPW)
C
 999  END
