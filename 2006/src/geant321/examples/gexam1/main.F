*
* $Id: main.F,v 1.1.1.1 1995/10/24 10:22:09 cernlib Exp $
*
* $Log: main.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:09  cernlib
* Geant
*
*
#include "geant321/pilot.h"
#if !defined(CERNLIB_INTER)
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      PROGRAM GEXAM1
C.
C.
C.         TEST PROGRAM FOR GEANT/SHOWER STUDIES
C.
C.         This program has been used to make the GEANT/EGS
C.         comparison.
C.              (see CERN/DD/85/1)
C.
C.         Authors    R.Brun, M.Maire  *********
C.
      COMMON/PAWC/H(10000)
      COMMON/GCBANK/Q(50000)
*
      CALL TIMEST(1E5)
*
      CALL GZEBRA(50000)
      CALL HLIMIT(-10000)
*
* ***          GEANT initialisation
      CALL UGINIT
 
*
* ***          Start events processing
      CALL GRUN
*
* ***          End of RUN
      CALL UGLAST
 
      END
#endif
