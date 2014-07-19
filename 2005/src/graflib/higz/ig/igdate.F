*
* $Id: igdate.F,v 1.4 1998/11/06 16:30:38 couet Exp $
*
* $Log: igdate.F,v $
* Revision 1.4  1998/11/06 16:30:38  couet
* - Should pass Y2K without problems ...
*
* Revision 1.3  1998/01/30 15:22:17  couet
* - APOLLO version removed
*
* Revision 1.2  1997/03/14 14:23:14  mclareni
* WNT mods
*
* Revision 1.1.1.1.2.1  1997/01/21 11:25:27  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.14/11 30/06/92  11.01.57  by  O.Couet
*-- Author :    O.Couet   31/10/90
      SUBROUTINE IGDATE(CHDT)
*.===========>
*.
*.   Gives the date
*.
*. _Output parameters:
*.
*.  CHARACTER CHDT : Date
*.
*..==========> (O.Couet)
      COMMON /SLATE/ ISL(40)
      CHARACTER*8 DAYDAT,DAYTIM
      CHARACTER*(*) CHDT
*.______________________________________
*
      CALL DATIME(ID,IT)
      IYEAR  = ISL(1)
      IMONTH = ISL(2)
      IDAY   = ISL(3)
      IHOUR  = ISL(4)
      IMIN   = ISL(5)
*
      CHDT = ' '
      WRITE (CHDT(1:4),'(I4.4)') IYEAR
      CHDT(5:5) = '/'
      WRITE (CHDT(6:7),'(I2.2)') IMONTH
      CHDT(8:8) = '/'
      WRITE (CHDT(9:10),'(I2.2)') IDAY
      WRITE (CHDT(14:15),'(I2.2)') IHOUR
      CHDT(16:16) = '.'
      WRITE (CHDT(17:18),'(I2.2)') IMIN
*
      END
