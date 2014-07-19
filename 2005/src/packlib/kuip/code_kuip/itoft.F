*
* $Id: itoft.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: itoft.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
#if defined(CERNLIB_NEWLIB)
*CMZ :  1.68/00 20/02/92  15.44.54  by  Fons Rademakers
*-- Author :    Harald Butenschoen   06/02/91
      SUBROUTINE ITOFT (IVAL,VALC,IERR)
************************************************************************
*                                                                      *
*                                                                      *
* Converts the integer IVAL to the character string VALC of length 8   *
* for constructing a FTxxF001 number                                   *
*                                                                      *
*     Input:  IVAL  integer number                                     *
*                                                                      *
*     Output: VALC  FTxxF001 string  with xx=ival                      *
*                                                                      *
*     Errors: IERR <> 0 error during conversion                        *
*                                                                      *
************************************************************************
      CHARACTER *(*) VALC
      INTEGER IVAL,IERR
*
      CHARACTER * 32 TT
      INTEGER I,J,NCSTR,NCH,LENOCC
*
      IERR = 0
      WRITE (TT,'(I32)',ERR=20 ) IVAL
      J = 0
      DO 10 I = 1, LENOCC(TT)
         IF (TT(I:I).EQ.' ') GO TO 10
         J = J + 1
         TT(J:J) = TT(I:I)
   10 CONTINUE
      NCSTR = J
      TT = TT(1:NCSTR)
*
      NCH = LEN (VALC)
      IF (NCH.LT.NCSTR) IERR = -1
      IF (NCSTR .EQ. 1) THEN
        VALC='0'//TT
        TT=VALC
        NCSTR=NCSTR+1
      ENDIF
      VALC='FT'//TT(1:NCSTR)//'F001'
      GO TO 999
*
   20 IERR = 1
*
  999 END

#endif
