*
* $Id: imupdwi.F,v 1.1.1.1 1996/02/14 13:11:01 mclareni Exp $
*
* $Log: imupdwi.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:01  mclareni
* Higz
*
*
#if defined(CERNLIB_MACMPW)
#include "higz/pilot.h"
*CMZ :  1.16/01 18/08/92  11.02.33  by  O.Couet
*-- Author : Cees de Laat
      subroutine imupdwi
c+
c       subroutine imupdwi
c
c the window is updated and moved to the top of the stack.
c-
      call imacplo(0,0,-999)
      end
#endif
