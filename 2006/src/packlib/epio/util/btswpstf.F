*
* $Id: btswpstf.F,v 1.1.1.1 1996/03/08 15:21:48 mclareni Exp $
*
* $Log: btswpstf.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:48  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if (defined(CERNLIB_STF77))&&(!defined(CERNLIB_STF77VX))
      SUBROUTINE BTSWAP(SOURCE,NBYTES)
      INTEGER SOURCE(*)
C
C  STF77 version to do BTSWAP with NO external calls.
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
#include "epio/wordsize.inc"
C
      IF(NBYTES .LE. 1) GO TO 999
C
C
C                  SOURCE word
      IWSRC =  0
      NBITS =  NBITPW-16
      NBITR =  0
C
      DO 10 IW16=1,NBYTES/2
C                 Update indices
      IF(NBITR.EQ.0) THEN
         NBITR = NBITPW - 16
         IWSRC = IWSRC + 1
      ELSE
         NBITR = NBITR - 16
      ENDIF
C
C          Shift 1st 16 bit word to the right and then bytes within
      SOURCE(IWSRC) = ISHFTC(ISHFTC(SOURCE(IWSRC),NBITS,NBITPW),8,16)
C
 10   CONTINUE
C          Shift last word if necessary
      SOURCE(IWSRC) = ISHFTC(SOURCE(IWSRC),NBITPW-NBITR,NBITPW)
C
 999  END
#endif
