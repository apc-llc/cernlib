*
* $Id: hnsize.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hnsize.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/08 07/09/93  19.35.57  by  Fons Rademakers
*-- Author :    Fons Rademakers   28/01/93
      SUBROUTINE HNSIZE(ICOL, LCOL, CCOL)
*.==========>
*.
*.           Returns the number of columns currently stored in the
*.           Ntuple. ICOL is the number of real, (unsigned) integer
*.           and logical columns, LCOL is the number of real*8 columns
*.           and CCOL is the number of character variable columns.
*.           This routine is used in HNTNAM to see if the user does not
*.           define more columns than can be analyzed by PAW. Hbook itself
*.           does not impose any Ntuple size limits.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcbook.inc"
*
      INTEGER   ICOL, LCOL, CCOL
      LOGICAL   LDUM
*
*-- save block address (not in link area but in this routine no
*-- relocation should happen so it will be ok)
*
      LOLDBL = LBLOK
*
      LBLOK  = LQ(LCID-1)
*
      ICOL = 0
      LCOL = 0
      CCOL = 0
*
*-- loop over all blocks
*
   10 LNAME = LQ(LBLOK-1)
*
      NDIM = IQ(LBLOK+ZNDIM)
      IOFF = 0
*
      DO 20 I = 1, NDIM
*
         CALL HNDESC(IOFF, NSUB, ITYPE, ISIZE, NBITS, LDUM)
         IELEM = 1
         DO 25 J = 1, NSUB
            LP = IQ(LINT+IQ(LNAME+IOFF+ZARIND)+(J-1))
            IF (LP .LT. 0) THEN
               IE = -LP
            ELSE
               LL = IQ(LNAME+LP-1+ZRANGE)
               IE = IQ(LINT+LL+1)
            ENDIF
            IELEM = IELEM*IE
   25    CONTINUE
*
         IOFF = IOFF + ZNADDR
*
         IF (ITYPE .NE. 5) THEN
            IF (ISIZE .EQ. 4) THEN
               ICOL = ICOL + IELEM
            ELSEIF (ISIZE .EQ. 8) THEN
               LCOL = LCOL + IELEM
            ENDIF
         ELSE
            CCOL = CCOL + IELEM
         ENDIF
   20 CONTINUE
*
      LBLOK = LQ(LBLOK)
      IF (LBLOK .NE. 0) GOTO 10
*
*-- restore block address
*
      LBLOK = LOLDBL
      LNAME = LQ(LBLOK-1)
*
      END
