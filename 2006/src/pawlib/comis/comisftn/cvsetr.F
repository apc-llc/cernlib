*
* $Id: cvsetr.F,v 1.1.1.1 1996/02/26 17:16:47 mclareni Exp $
*
* $Log: cvsetr.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:47  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_LIB)
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CVSETR(A,N,VALUE)
      CALL VFILL(A,N,VALUE)
      END
#endif
