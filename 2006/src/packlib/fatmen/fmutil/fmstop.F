*
* $Id: fmstop.F,v 1.1.1.1 1996/03/07 15:18:17 mclareni Exp $
*
* $Log: fmstop.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:17  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSTOP
*CMZ :          08/05/91  11.30.21  by  Jamie Shiers
*-- Author :    Jamie Shiers   08/05/91
      PRINT *,'FMSTOP. terminating due to fatal error.'
      STOP
      END
