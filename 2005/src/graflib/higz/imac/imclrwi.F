*
* $Id: imclrwi.F,v 1.1.1.1 1996/02/14 13:11:00 mclareni Exp $
*
* $Log: imclrwi.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:00  mclareni
* Higz
*
*
#if defined(CERNLIB_MACMPW)
#include "higz/pilot.h"
*CMZ :  1.23/06 01/11/95  14.04.54  by  O.Couet
*-- Author : Cees de Laat
      subroutine imclrwi
c+
c       subroutine imclrwi
c
c Clears the window. If the window is not open, it will be opened.
c-
      call imacplo(ima,idm,10)
      call imacplo(ima,0,-2)
      end
#endif
