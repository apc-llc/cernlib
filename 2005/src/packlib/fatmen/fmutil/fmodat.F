*
* $Id: fmodat.F,v 1.1.1.1 1996/03/07 15:18:17 mclareni Exp $
*
* $Log: fmodat.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:17  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMODAT(IDATE,ITIME,ODATE)
*
*     Routine to convert integer date and time to character format
*     for inserting into ORACLE / SQL tables
*     IDATE = YYMMDD, ITIME = HHMM (I*4)
*     ODATE = DD-MMM-YY HH:MM (24 hour clock)
*     Insert into ORACLE /SQL with TO_DATE(odate,'DD-MON-YY HH24:MI')
*
      CHARACTER*15 ODATE
      CHARACTER*3  MONTH(12)
 
      DATA MONTH(1)/'JAN'/,
     +     MONTH(2)/'FEB'/,
     +     MONTH(3)/'MAR'/,
     +     MONTH(4)/'APR'/,
     +     MONTH(5)/'MAY'/,
     +     MONTH(6)/'JUN'/,
     +     MONTH(7)/'JUL'/,
     +     MONTH(8)/'AUG'/,
     +     MONTH(9)/'SEP'/,
     +     MONTH(10)/'OCT'/,
     +     MONTH(11)/'NOV'/,
     +     MONTH(12)/'DEC'/
 
      IF ((ITIME .EQ. 0) .OR. (IDATE .EQ. 0)) CALL DATIME(IDATE,ITIME)
      IMIN = MOD(ITIME,100)
      IHOU = ITIME/100
*
      IDAY = MOD(IDATE,100)
      IYEA = IDATE/100
      IMON = MOD(IYEA,100)
      IYEA = IYEA/100
*
      WRITE(ODATE,9001) IDAY,MONTH(IMON),IYEA,IHOU,IMIN
9001  FORMAT(I2,'-',A3,'-',I2,' ',I2,':',I2)
 
      DO 10 I=1,15
      IF (I .EQ. 10) GOTO 10
      IF (ODATE(I:I) .EQ. ' ') ODATE(I:I) = '0'
10    CONTINUE
*
      END
