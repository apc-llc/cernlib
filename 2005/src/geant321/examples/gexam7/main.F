*
* $Id: main.F,v 1.1.1.1 1995/10/24 10:22:20 cernlib Exp $
*
* $Log: main.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:20  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
C.    *
C.    *       TEST PROGRAM FOR GEANT/SHOWER STUDIES
C.    *
C.    *       Authors    R.Brun, M.Maire, P.Ribaric  *********
C.
      COMMON/PAWC/H(10000)
      COMMON/GCBANK/Q(50000)
C.
C.
      CALL GZEBRA(50000)
      CALL HLIMIT(-10000)
C
C ***          GEANT initialisation
C
      CALL UGINIT
C
C ***          Start events processing
C
      CALL GRUN
C
C ***          End of RUN
C
      CALL UGLAST
C.
      STOP
      END
