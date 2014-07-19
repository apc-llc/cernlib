*
* $Id: hitoc.F,v 1.1.1.1 1996/01/16 17:07:57 mclareni Exp $
*
* $Log: hitoc.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:57  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/00 28/01/92  17.40.00  by  Fons Rademakers
*-- Author :    Fons Rademakers   28/01/92
      SUBROUTINE HITOC(IVAL, VALC, NCSTR, IERR)
*.==========>
*.
*. Converts the integer IVAL to the character string VALC
*.
*.     Input:  IVAL  integer number
*.
*.     Output: VALC  character string
*.             NCSTR length of VALC
*.
*.     Errors: IERR <> 0 error during conversion
*.
*..=========> ( A.A.Rademakers )
*
      CHARACTER*(*) VALC
      INTEGER       IVAL, IERR
*
      CHARACTER*32  TT
      INTEGER       I, J, NCSTR, NCH, LENOCC
*
      IERR = 0
      WRITE(TT,'(I32)',ERR=20) IVAL
      J = 0
      DO 10 I = 1, LENOCC(TT)
         IF (TT(I:I) .EQ. ' ') GOTO 10
         J = J + 1
         TT(J:J) = TT(I:I)
   10 CONTINUE
      NCSTR = J
      TT = TT(1:NCSTR)
*
      NCH = LEN(VALC)
      IF (NCH .LT. NCSTR) IERR = -1
      VALC(1:NCH) = TT
      GOTO 999
*
   20 IERR = 1
*
  999 END
