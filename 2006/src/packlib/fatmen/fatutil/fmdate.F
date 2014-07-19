*
* $Id: fmdate.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fmdate.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      INTEGER FUNCTION FMDATE(JULIAN)
*CMZ :          22/01/91  13.35.07  by  Jamie Shiers
*-- Author :    Jamie Shiers   22/01/91
*
*     Function to convert julian date to YYMMDD form
*
      INTEGER NDAYS(12),NLEAP(12)
 
      DATA NDAYS/00,31,59,90,120,151,181,212,243,273,304,334/
      DATA NLEAP/00,31,60,91,121,152,182,213,244,274,305,335/
*
      FMDATE = 0
 
      IYEA = JULIAN/1000
      ITMP = JULIAN - IYEA * 1000
 
      IF(MOD(IYEA,4).EQ.0) THEN
         DO 10 I=1,11
            IF(ITMP.LT.NLEAP(I+1)) GOTO 20
   10    CONTINUE
         I = 12
   20    CONTINUE
         IMON = I
         IF(IMON.GT.1) THEN
            IDAY = ITMP - NLEAP(I-1)
         ELSE
            IDAY = ITMP
         ENDIF
      ELSE
         DO 30 I=1,11
            IF(ITMP.LT.NDAYS(I+1)) GOTO 40
   30    CONTINUE
         I = 12
   40    CONTINUE
         IMON = I
         IF(IMON.GT.1) THEN
            IDAY = ITMP - NDAYS(I-1)
         ELSE
            IDAY = ITMP
         ENDIF
      ENDIF
      END
