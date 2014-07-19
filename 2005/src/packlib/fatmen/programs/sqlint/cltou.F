*
* $Id: cltou.F,v 1.1.1.1 1996/03/07 15:17:49 mclareni Exp $
*
* $Log: cltou.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:49  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_QX_SC)
      SUBROUTINE CLTOU(CHSTR)
*CMZ :          29/04/91  17.34.54  by  Jamie Shiers
*-- Author :    Jamie Shiers   29/04/91
*
*     Needed because CERNLIB compiled with -qextname (RS6000)
*     but ORACLE libraries are compiled without this
*     option. (FATMEN<->ORACLE interface routines must
*     therefore be compiled without -qextname)
*
*     HP/UX : no +ppu
*
      CHARACTER*(*)    CHSTR
      CALL CLTOU_(CHSTR)
      END
#endif
