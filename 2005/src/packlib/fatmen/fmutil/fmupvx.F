*
* $Id: fmupvx.F,v 1.1.1.1 1996/03/07 15:18:18 mclareni Exp $
*
* $Log: fmupvx.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:18  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMUPVX(CHDATE,ID,IT,IP,IRC)
*CMZ :          11/07/91  17.12.06  by  Jamie Shiers
*-- Author :    Jamie Shiers   11/07/91
*
*     Unpack date & time into VAX format
*     11-JUL-1991 17:14:41.37
*     dd-mmm-yyyy hh:mm:ss
*     123456789_123456789_
*
      CHARACTER*(*) CHDATE
      CHARACTER*3   MONTHS(12),CHMON
      DATA          MONTHS( 1)/'JAN'/,MONTHS( 2)/'FEB'/,
     +              MONTHS( 3)/'MAR'/,MONTHS( 4)/'APR'/,
     +              MONTHS( 5)/'MAY'/,MONTHS( 6)/'JUN'/,
     +              MONTHS( 7)/'JUL'/,MONTHS( 8)/'AUG'/,
     +              MONTHS( 9)/'SEP'/,MONTHS(10)/'OCT'/,
     +              MONTHS(11)/'NOV'/,MONTHS(12)/'DEC'/
 
*
*     Unpack from FATMEN bank...
*
      CALL FMUPTM(ID,IT,IP,IRC)
 
      IYEA = 1900 + ID/10000
      IMON = MOD(ID/100,100)
      IDAY = MOD(ID,100)
      IHOU = IT/100
      IMIN = MOD(IT,100)
 
      WRITE(CHDATE,9001) IDAY,MONTHS(IMON),IYEA,IHOU,IMIN
9001  FORMAT(I2.2,'-',A3,'-',I4,' ',I2.2,':',I2.2)
      CHDATE(18:23) = ':00.00'
 
      END
