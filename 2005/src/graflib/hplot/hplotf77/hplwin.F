*
* $Id: hplwin.F,v 1.1.1.1 1996/01/19 10:50:12 mclareni Exp $
*
* $Log: hplwin.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:12  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.17/07 27/09/93  11.23.40  by  O.Couet
*-- Author :
      SUBROUTINE HPLWIN(IX,IY,IW,KIC)
      CALL IGERR('HPLWIN is now called HPLZON',' ')
      CALL HPLZON(IX,IY,IW,KIC)
      END
