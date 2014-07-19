*
* $Id: mkchar.F,v 1.2 1996/04/02 22:15:06 thakulin Exp $
*
* $Log: mkchar.F,v $
* Revision 1.2  1996/04/02 22:15:06  thakulin
* Support for EPC Fortran:  remove char-int equivalences and use F90
* transfer facility.
*
* Revision 1.1.1.1  1996/02/26 17:16:50  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :          23/10/95  12.42.56  by  Julian Bunn
*-- Author : V.Berezhnoi
      FUNCTION MKCHAR(J)
***----------------------------------------------
*   gets character from j-byte address (say X)
*   puts ichar(X) into MKCHAR
***----------------------------------------------
#include "comis/cspar.inc"
#include "comis/comis.inc"
#include "comis/cspnts.inc"
      CHARACTER *4 T
#if !defined(CERNLIB_F90) && !defined(CERNLIB_QFEPC)
      EQUIVALENCE (I,T)
#endif
#if defined(CERNLIB_APOLLO)
      IND= RSHFT (J,2)
#endif
#if defined(CERNLIB_UNIX)
      IND= ISHFT (J,-2)
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_IBM)||defined(CERNLIB_WINNT)
      IND=J/4
#endif
      NB=J-IND*4+1
*      IV=IND-JTOPA
#if defined(CERNLIB_F90) || defined(CERNLIB_QFEPC)
      T=transfer(IA(IND-JTOPA),T)
#else
      I=IA(IND-JTOPA)
#endif
      MKCHAR=ICHAR(T(NB:NB))
      END
