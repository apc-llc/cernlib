*
* $Id: mdigit.F,v 1.1.1.1 1996/02/26 17:16:58 mclareni Exp $
*
* $Log: mdigit.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:58  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)||!defined(CERNLIB_ASSEMB)
*CMZ :  1.18/00 17/02/94  17.43.18  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION MDIGIT(I)
#include "comis/csichv.inc"
      MDIGIT=I-ICHD0
      END
#endif
