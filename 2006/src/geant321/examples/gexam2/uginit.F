*
* $Id: uginit.F,v 1.2 1996/02/01 14:54:52 ravndal Exp $
*
* $Log: uginit.F,v $
* Revision 1.2  1996/02/01 14:54:52  ravndal
* Clean up the repository
*
* Revision 1.1.1.1  1995/10/24 10:22:11  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE UGINIT
*
************************************************************************
*                                                                      *
*              To initialise GEANT3 program and read data cards        *
*                                                                      *
************************************************************************
*
#include "geant321/gcunit.inc"
#include "geant321/gckine.inc"
#include "cdhscom.inc"
*
*     -----------------------------------------------------------------
*
*             Open user files
*
      CALL UFILES
*
*             Initialize GEANT
*
      CALL GINIT
*
*             Prints version number
*
      WRITE(LOUT,1000)
*
*             Set default values in /CDHSCOM/
*             Define additional data cards to modigy them
*
      IKINE  = 9
      PKINE(1)  = 140.0
      SIGBX  = 2.50
      SIGBY  = 1.00
      SIGP   = 0.025
      EMUON   = 1.0
      TRESHO = 0.6
      TRIGGE = 3.0
      CALL FFKEY('SIGB', SIGBX, 2, 'REAL')
      CALL FFKEY('SIGP',  SIGP, 1, 'REAL')
      CALL FFKEY('MUON', EMUON, 1, 'REAL')
      CALL FFKEY('TRHS', TRESHO,2, 'REAL')
*
*             Read data cards with FFREAD
*
      CALL GFFGO
*
*             Initialize GEANT/ZEBRA data structures
*
      CALL GZINIT
*
#if defined(CERNLIB_HIGZ)
*
*             Initialize graphics package
*
      CALL GDINIT
#endif
*
*             Geometry and materials description.
*
      CALL GECDHS
*
*             Particle table definition and energy loss initialization.
*
      CALL GPART
      CALL GPHYSI
*
#if defined(CERNLIB_HIGZ)
*
*             Create a view bank
*
      CALL VIEWYZ(1)
#endif
      CALL UINIT
*
 1000 FORMAT(/,'  GEXAM2 VERSION 1.00 : ',/)
      END
