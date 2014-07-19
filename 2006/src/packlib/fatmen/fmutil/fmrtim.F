*
* $Id: fmrtim.F,v 1.1.1.1 1996/03/07 15:18:17 mclareni Exp $
*
* $Log: fmrtim.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:17  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRTIM(ELAPSD)
*CMZ :          10/04/91  09.13.47  by  Jamie Shiers
*-- Author :    Jamie Shiers   10/04/91
*
********************************************************************************
*
* Routine functionally equivalent to TIMED, but for real time
*
* (copied from KUIP)
*
********************************************************************************
*
      CHARACTER*8 ELAPSD
      INTEGER ND(2),NT(2)
      CHARACTER*8 CD,CT
      CHARACTER*(*) DIGIT
      PARAMETER (DIGIT='0123456789')
      INTEGER MONTH(0:12)
      SAVE IOLDAY,IOLSEC
      DATA IOLDAY,IOLSEC /0,0/
      DATA MONTH /0,31,59,90,120,151,181,212,243,273,304,334,365/
*
      CALL DATIMH(ND,NT)
      CALL UHTOC(ND,4,CD,8)
      CALL UHTOC(NT,4,CT,8)
*
* Convert date into number of days from start of year
*
      NHIGH=INDEX(DIGIT,CD(1:1))-1
      NLOW=INDEX(DIGIT,CD(2:2))-1
      NDAY=NHIGH*10+NLOW
 
      NHIGH=INDEX(DIGIT,CD(4:4))-1
      NLOW=INDEX(DIGIT,CD(5:5))-1
      NMONTH=NHIGH*10+NLOW
 
      NHIGH=INDEX(DIGIT,CD(7:7))-1
      NLOW=INDEX(DIGIT,CD(8:8))-1
      NYEAR=NHIGH*10+NLOW
 
      IDAY=NDAY-1+MONTH(NMONTH-1)
      IF ((NYEAR/4)*4.EQ.NYEAR.AND.NMONTH.GT.2) IDAY=IDAY+1
*
* Convert time into number of seconds from start of day
*
      NHIGH=INDEX(DIGIT,CT(1:1))-1
      NLOW=INDEX(DIGIT,CT(2:2))-1
      NHOUR=NHIGH*10+NLOW
 
      NHIGH=INDEX(DIGIT,CT(4:4))-1
      NLOW=INDEX(DIGIT,CT(5:5))-1
      NMINUT=NHIGH*10+NLOW
 
      NHIGH=INDEX(DIGIT,CT(7:7))-1
      NLOW=INDEX(DIGIT,CT(8:8))-1
      NSECON=NHIGH*10+NLOW
 
      ISEC=NSECON+NMINUT*60+NHOUR*3600
*
* Compute the elapsed time from previous call (in HH:MM:SS format)
*
      IDELAP=IDAY-IOLDAY
      ISELAP=ISEC-IOLSEC
      IOLSEC=ISEC
      IOLDAY=IDAY
      ISEC=MOD(ISELAP,60)
      IMINUT=(MOD(ISELAP,3600)-ISEC)/60
      IHOUR=ISELAP/3600+IDELAP*24
      IF (IHOUR.GT.99) THEN
         WRITE(ELAPSD,1100) IMINUT,ISEC
      ELSE
         WRITE(ELAPSD,1000) IHOUR,IMINUT,ISEC
      ENDIF
 
1000  FORMAT(I2.2,':',I2.2,':',I2.2)
1100  FORMAT('**:',I2.2,':',I2.2)
 
      END
