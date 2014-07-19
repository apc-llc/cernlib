*
* $Id: imgetge.F,v 1.1.1.1 1996/02/14 13:11:00 mclareni Exp $
*
* $Log: imgetge.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:00  mclareni
* Higz
*
*
#if defined(CERNLIB_MACMPW)
#include "higz/pilot.h"
*CMZ :  1.14/09 05/06/92  09.51.11  by  O.Couet
*-- Author : Cees de Laat
      subroutine imgetge(iwid,ix,iy,iw,ih)
c+
c       subroutine imgetge(iwid,ix,iy,iw,ih)
c
c iwid smaller than zero, returns display size, otherwise open window size.
c-
      if(iwid.lt.0)then
         call imgetds(ix,iy,iw,ih)
      else
         call imacplo(ix,iy,-14)
         call imacplo(iw,ih,-4)
      endif
      end
#endif
