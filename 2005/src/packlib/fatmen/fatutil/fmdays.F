*
* $Id: fmdays.F,v 1.2 1998/02/13 14:01:20 jamie Exp $
*
* $Log: fmdays.F,v $
* Revision 1.2  1998/02/13 14:01:20  jamie
* Y2k mods
*
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMDAYS(GENAME,LBANK,KEYS,NDAYS,CHOPT,IRC)
*CMZ :          22/01/91  14.11.49  by  Jamie Shiers
*-- Author :    Jamie Shiers   22/01/91
      CHARACTER*(*) GENAME
      DIMENSION     NDAYS(3),NPACK(3)
*
*     On return, NDAYS(1) = number of days since creation date
*                NDAYS(2) = number of days since date catalogued
*                           (added to FATMEN catalogue)
*                NDAYS(3) = number of days since last used
*
*     IYEAR = YY; IYEARC = CCYY
*
#include "fatmen/slate.inc"
#include "fatmen/fmnkeys.inc"
      DIMENSION     KEYS(LKEYFA)
      INTEGER       FMJULI
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatopts.inc"
      IRC = 0
      LGN = LENOCC(GENAME)
      CALL DATIME(ID,IT)
      IYEARC = IS(1)
      IJ  = FMJULI(ID)
      ICENT = IS(1)/100
      IF(LBANK.EQ.0) THEN
         CALL FMGETK(GENAME(1:LGN),LBANK,KEYS,IC)
      ENDIF
      CALL VZERO(IQUEST(11),3)
      CALL UCOPY(IQ(LBANK+KOFUFA+MCRTFA),NPACK,3)
      DO 10 I=1,3
      IF(NPACK(I).EQ.0) THEN
         NDAYS(I) = -1
         IQUEST(10+I) = -1
         GOTO 10
      ENDIF
      IC = 0
      CALL FMUPTM(IDATE,ITIME,NPACK(I),IC)
      IF(IC.NE.0) THEN
         NDAYS(I) = -2
         IQUEST(10+I) = -2
         GOTO 10
      ENDIF
      JDATE = FMJULI(IDATE)
      IYEAR = IJ/1000
      ITEMP = IJ - IYEAR*1000
      JYEAR = JDATE/1000
      JTEMP = JDATE - JYEAR*1000
      IF(IYEAR.EQ.JYEAR) THEN
         NDAYS(I) = ITEMP - JTEMP
      ELSE
         NLEAP  = 0
         IF(JYEAR.LE.50) THEN
            JYEARC = 2000 + JYEAR
         ELSE
            JYEARC = 1900 + JYEAR
         ENDIF

         DO 20 J=IYEARC,JYEARC
         IF(MOD(J,4).EQ.0) NLEAP = NLEAP + 1
20       CONTINUE
         NDAYS(I) = ITEMP + (IYEARC-JYEARC-1)*365 + 365-JTEMP + NLEAP
      ENDIF
10    CONTINUE
      END
