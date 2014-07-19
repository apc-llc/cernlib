*
* $Id: imsetln.F,v 1.1.1.1 1996/02/14 13:11:01 mclareni Exp $
*
* $Log: imsetln.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:01  mclareni
* Higz
*
*
#if defined(CERNLIB_MACMPW)
#include "higz/pilot.h"
*CMZ :  1.16/04 02/10/92  13.36.10  by  O.Couet
*-- Author : Cees de Laat
      subroutine imsetln(iwidth)
c+
c       subroutine imsetln(iwidth)
c
c Set line width in pixels.
c-
      call imacplo(iwidth,iwidth,9)
      end
#endif
