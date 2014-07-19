*
* $Id: viewyz.F,v 1.1.1.1 1995/10/24 10:22:16 cernlib Exp $
*
* $Log: viewyz.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:16  cernlib
* Geant
*
*
#include "geant321/pilot.h"
#if defined(CERNLIB_HIGZ)
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE VIEWYZ (IVIEW)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *     Draw full set up in 'view bank' mode.                      *
C.    *                                                                *
C.    ******************************************************************
C.
      CALL GSATT('*','SEEN', 1)
C
      CALL GSATT('MOD1','SEEN', -2)
      CALL GSATT('MOD2','SEEN', -2)
      CALL GSATT('MOD3','SEEN', -2)
C
C             Create bank for view YZ.
C
      CALL GDOPEN(IVIEW)
C
      CALL GDRAWC ('CALO', 1, 0., 10.,10.,0.08,0.2)
C
      CALL GDCLOS
C
      END
#endif
