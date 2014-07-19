*
* $Id: csetc.F,v 1.1.1.1 1996/02/26 17:16:58 mclareni Exp $
*
* $Log: csetc.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:58  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)||!defined(CERNLIB_ASSEMB)
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSETC(I1,I2)
      CALL CSCHAR(MJCHAR(I1),I2)
      RETURN
      END
#endif
