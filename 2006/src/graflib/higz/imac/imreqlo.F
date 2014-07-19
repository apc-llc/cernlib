*
* $Id: imreqlo.F,v 1.1.1.1 1996/02/14 13:11:00 mclareni Exp $
*
* $Log: imreqlo.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:00  mclareni
* Higz
*
*
#if defined(CERNLIB_MACMPW)
#include "higz/pilot.h"
*CMZ :  1.16/01 18/08/92  10.44.19  by  O.Couet
*-- Author : Cees de Laat
      integer function imreqlo(mode,ityp,ix,iy)
c+
c       integer function imreqlo(mode,ityp,ix,iy)
c locator request, always crosshair,
c mode = 0 normal mode
c mode = 1 sampling mode
c-
      if(mode.eq.0)then
         call imacplo(ix,iy,-3)
         imreqlo=2
      elseif(mode.eq.1)then
         call imacplo(ix,iy,-13)
         call imgetms(imreqlo)
      endif
      end
#endif
