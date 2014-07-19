*
* $Id: fmpkvx.F,v 1.1.1.1 1996/03/07 15:18:18 mclareni Exp $
*
* $Log: fmpkvx.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:18  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPKVX(CHDATE,ID,IT,IP,IRC)
*
*     Pack VAX format date & time
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
 
      READ(CHDATE,'(I2,1X,A3,1X,I4,1X,I2,1X,I2)')
     +     IDAY,CHMON,IYEA,IHOU,IMIN
      IMON   = ICNTH(CHMON,MONTHS,12)
 
      IYEA   = MOD(IYEA,1900)
 
      ID     = IYEA*10000 + IMON*100 + IDAY
      IT     = IHOU*100   + IMIN
*
*     and pack for insertion into FATMEN bank...
*
      CALL FMPKTM(ID,IT,IP,IRC)
 
      END
