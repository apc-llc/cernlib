*
* $Id: cschar.F,v 1.2 1996/04/02 22:15:01 thakulin Exp $
*
* $Log: cschar.F,v $
* Revision 1.2  1996/04/02 22:15:01  thakulin
* Support for EPC Fortran:  remove char-int equivalences and use F90
* transfer facility.
*
* Revision 1.1.1.1  1996/02/26 17:16:50  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 04/03/94  16.11.47  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSCHAR(J,NH)
***----------------------------------------------
*   puts char(NH) into j-byte address
***----------------------------------------------
#include "comis/cspar.inc"
#include "comis/comis.inc"
#include "comis/cspnts.inc"
#if defined(CERNLIB_APOLLO)
      CHARACTER *4 T
      EQUIVALENCE (I,T)
      IND= RSHFT (J,2)
      NB=J-IND*4+1
      IV=IND-JTOPA
      I=IA(IV)
      T(NB:NB)=CHAR(NH)
      IA(IV)=I
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_DECS))
      CHARACTER *4 T
# if !defined(CERNLIB_F90) && !defined(CERNLIB_QFEPC)
      EQUIVALENCE (I,T)
# endif
      IND= ISHFT (J,-2)
      NB=J-IND*4+1
      IV=IND-JTOPA
# if defined(CERNLIB_F90) || defined(CERNLIB_QFEPC)
      T=transfer(IA(IV),T)
# else
      I=IA(IV)
# endif
      T(NB:NB)=CHAR(NH)
# if defined(CERNLIB_F90) || defined(CERNLIB_QFEPC)
      IA(IV)=transfer(T,I)
# else
      IA(IV)=I
# endif
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS)||defined(CERNLIB_IBM)
      CHARACTER *4 T
      EQUIVALENCE (I,T)
      IND=J/4
      NB=J-IND*4+1
      IV=IND-JTOPA
      I=IA(IV)
      T(NB:NB)=CHAR(NH)
      IA(IV)=I
#endif
      END
