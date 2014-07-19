*
* $Id: viewyz.F,v 1.2 1996/02/22 13:27:00 ravndal Exp $
*
* $Log: viewyz.F,v $
* Revision 1.2  1996/02/22 13:27:00  ravndal
* Cleaning up CARTOCVS interface
*
* Revision 1.1.1.1  1995/10/24 10:22:23  cernlib
* Geant
*
*
#include "geant321/pilot.h"
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
#include "pvolum.inc"
 
      CALL GSATT('*','SEEN', 0)
      CALL GSATT('ECAL','SEEN', 1)
C
C             Create bank for view YZ.
C
      CALL GDOPEN(IVIEW)
C
      SX=0.9*10./ZECAL
      SY=SX
      CALL GDRAWC ('ECAL', 1, 0., 10.,10.,SX,SY)
C
      CALL GDCLOS
C
      END
 
