*
* $Id: viewyz.F,v 1.1.1.1 1995/10/24 10:22:12 cernlib Exp $
*
* $Log: viewyz.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:12  cernlib
* Geant
*
*
#include "geant321/pilot.h"
#if defined(CERNLIB_HIGZ)
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE VIEWYZ (IVIEW)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *     Draw full set up in 'view bank' mode.                      *
C.    *                                                                *
C.    ******************************************************************
C.
      CALL GSATT('*','SEEN', 0)
C
      CALL GSATT('SINT','SEEN', 1)
C
C             Create bank for view YZ.
C
      CALL GDOPEN(IVIEW)
C
      CALL GDHEAD(110110, 'VIEW YZ$' ,0.5)
C
      CALL GDRAWC ('CDHS', 1, 0., 10.,10.,0.05,0.04)
C
      CALL GDCLOS
C
      END
#endif
