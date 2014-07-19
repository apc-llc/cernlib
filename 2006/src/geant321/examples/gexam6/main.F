*
* $Id: main.F,v 1.1.1.1 1995/10/24 10:22:19 cernlib Exp $
*
* $Log: main.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:19  cernlib
* Geant
*
*
#include "geant321/pilot.h"
#if !defined(CERNLIB_INTER)
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
C.
C.    *
C.    *       PROGRAM FOR GEANT SHOWER LIBRARY GENERATION
C.    *
C     *      Authors : Pal Ribaric (Budapest), Michel Maire (Annecy)
C.
      PARAMETER (NZEBRA =50000, NHBOOK=10000)
      COMMON/GCBANK/Q(NZEBRA)
      COMMON/PAWC/H(NHBOOK)
C.
C.
      CALL GZEBRA(NZEBRA)
      CALL HLIMIT(-NHBOOK)
C
C *** GEANT initialisation
C
      CALL UGINIT
C
C *** Start events processing
C
      CALL GRUN
C
C *** End of RUN
C
      CALL UGLAST
 
      STOP
      END
#endif
