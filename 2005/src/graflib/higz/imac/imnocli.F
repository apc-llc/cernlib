*
* $Id: imnocli.F,v 1.1.1.1 1996/02/14 13:11:00 mclareni Exp $
*
* $Log: imnocli.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:00  mclareni
* Higz
*
*
#if defined(CERNLIB_MACMPW)
#include "higz/pilot.h"
*CMZ :  1.16/04 02/10/92  13.35.41  by  O.Couet
*-- Author : Cees de Laat
      subroutine imnocli
c+
c       subroutine imnocli
c
c Remove clipping.
c-
      call imacplo(0,0,-8)
      end
#endif
