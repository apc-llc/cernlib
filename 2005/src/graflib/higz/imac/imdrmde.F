*
* $Id: imdrmde.F,v 1.1.1.1 1996/02/14 13:11:00 mclareni Exp $
*
* $Log: imdrmde.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:00  mclareni
* Higz
*
*
#if defined(CERNLIB_MACMPW)
#include "higz/pilot.h"
*CMZ :  1.14/09 05/06/92  09.51.11  by  O.Couet
*-- Author : Cees de Laat
      subroutine imdrmde(mode)
c+
c       subroutine imdrmde(mode)
c
c Set drawing mode
c mode = 1 = pixels are set
c mode = 2 = xor mode
c-
      if(mode.eq.1)then
         call imacplo(0,0,1)
      elseif(mode.eq.2)then
         call imacplo(0,0,2)
      endif
      end
#endif
