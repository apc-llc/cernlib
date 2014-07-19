*
* $Id: mntiny.F,v 1.1.1.1 1996/03/07 14:31:32 mclareni Exp $
*
* $Log: mntiny.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:32  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNTINY(EPSP1,EPSBAK)
#include "minuit/d506dp.inc"
CC        Compares its argument with the value 1.0, and returns
CC        the value .TRUE. if they are equal.  To find EPSMAC
CC        safely by foiling the Fortran optimizer
CC
      PARAMETER (ONE=1.0)
      EPSBAK =  EPSP1  - ONE
      RETURN
      END
