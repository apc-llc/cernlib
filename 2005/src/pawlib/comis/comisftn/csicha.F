*
* $Id: csicha.F,v 1.2 1996/04/02 22:15:04 thakulin Exp $
*
* $Log: csicha.F,v $
* Revision 1.2  1996/04/02 22:15:04  thakulin
* Support for EPC Fortran:  remove char-int equivalences and use F90
* transfer facility.
*
* Revision 1.1.1.1  1996/02/26 17:16:50  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSICHA(J,KOD)
      CHARACTER*4 T
#if !defined(CERNLIB_F90) && !defined(CERNLIB_QFEPC)
      EQUIVALENCE (T,I)
#endif
      T=CHAR(KOD)
#if defined(CERNLIB_F90) || defined(CERNLIB_QFEPC)
      I=transfer(T,I)
#endif
      CALL CSCHAR(J,I)
      END
