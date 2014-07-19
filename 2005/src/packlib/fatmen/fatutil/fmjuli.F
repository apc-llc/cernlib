*
* $Id: fmjuli.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fmjuli.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      INTEGER FUNCTION FMJULI(IDATE)
*CMZ :          22/01/91  13.35.07  by  Jamie Shiers
*-- Author :    Jamie Shiers   22/01/91
*
*     Function to convert date in form YYMMDD to julian format (YYDDD)
*
      INTEGER NDAYS(12),NLEAP(12)
 
      DATA NDAYS/00,31,59,90,120,151,181,212,243,273,304,334/
      DATA NLEAP/00,31,60,91,121,152,182,213,244,274,305,335/
*
      FMJULI = 0
      IDAY = MOD(IDATE,100)
      IYEA = IDATE/100
      IMON = MOD(IYEA,100)
      IYEA = IYEA/100
 
      IF(MOD(IYEA,4).EQ.0) THEN
         FMJULI = IYEA * 1000 + NLEAP(IMON) + IDAY
      ELSE
         FMJULI = IYEA * 1000 + NDAYS(IMON) + IDAY
      ENDIF
 
      END
