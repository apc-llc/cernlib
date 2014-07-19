*
* $Id: kidate.F,v 1.2 1998/11/10 13:42:19 couet Exp $
*
* $Log: kidate.F,v $
* Revision 1.2  1998/11/10 13:42:19  couet
* - Y2K
*
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.61/01 22/06/89  17.34.30  by
*-- Author :
      SUBROUTINE KIDATE(CHDATE,CHTIME)
*
********************************************************************************
*
* Get the current date and time
*
*     Output :
*
      CHARACTER*(*) CHDATE
      CHARACTER*(*) CHTIME
*
********************************************************************************
*
      COMMON /SLATE/ ISL(40)
      CALL DATIME(ID,IT)
      IYEAR  = ISL(1)
      IMONTH = ISL(2)
      IDAY   = ISL(3)
      IHOUR  = ISL(4)
      IMIN   = ISL(5)
      ISEC   = ISL(6)
*
      CHDATE = ' '
      WRITE (CHDATE(1:2),'(I2.2)') IDAY
      CHDATE(3:3) = '/'
      WRITE (CHDATE(4:5),'(I2.2)') IMONTH
      CHDATE(6:6) = '/'
      WRITE (CHDATE(7:10),'(I4.4)') IYEAR
*
      CHTIME = ' '
      WRITE (CHTIME(1:2),'(I2.2)') IHOUR
      CHDATE(3:3) = '.'
      WRITE (CHTIME(4:5),'(I2.2)') IMIN
      CHDATE(6:6) = '.'
      WRITE (CHTIME(7:8),'(I2.2)') ISEC
*
      END
