*
* $Id: uginit.F,v 1.2 1996/02/01 15:00:39 ravndal Exp $
*
* $Log: uginit.F,v $
* Revision 1.2  1996/02/01 15:00:39  ravndal
* Clean up repository
*
* Revision 1.1.1.1  1995/10/24 10:22:15  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UGINIT
*
************************************************************************
*                                                                      *
*          To initialise GEANT3 program and read data cards            *
*                                                                      *
************************************************************************
*
#include "geant321/gcbank.inc"
#include "geant321/gcflag.inc"
#include "geant321/gcunit.inc"
#include "urgeom.inc"
#include "urmips.inc"
*
*     -----------------------------------------------------------------
*
*             Open user files
*
      CALL UFILES
*
*             Initialize GEXAM4 global variables
*
      IGAST = 1
      IDSTW = 0
      NTXMIP= 0
      TXMIP = 0.
      TXMIP2= 0.
      CALL VZERO(XKPL,300)
*
*             Initialize GEANT
      CALL GINIT
*
*             Prints version number
      WRITE(LOUT,1000)
*
*             Define a data card 'GAST' to change gas type
*
      CALL FFKEY('GAST',IGAST,1,'INTEGER')
*
*             Define a data card to fill and write the PH matrix
*
      CALL FFKEY('DSTW',IDSTW,1,'INTEGER')
*
*             Read data cards with FFREAD
*
      CALL GFFGO
*
*             Initialize GEANT/ZBOOK data structures
      CALL GZINIT
*
#if defined(CERNLIB_HIGZ)
*
*             Initialize drawing package
      IF(ISWIT(7).EQ.0)THEN
         CALL GDINIT
      ENDIF
#endif
*
*             Geometry and materials description.
      CALL UGEOM
*
*             Particle table definition and energy loss initialization.
      CALL GPART
      CALL GPHYSI
*
*             Compute calibration factors for the 2 types of chambers.
*             Calibration is obtained with minimum ionizing muons
*             of 500 MeV in material 5 (Argon/Isobuthane or Argon/CO2)
*
      JMA    = LQ(JMATE-5)
      JMULOS = LQ(JMA-2)
      DEDX   = Q(JMULOS+45)
      CALIB1 = DEDX*WGS1
      CALIB2 = DEDX*WGS2
*
#if defined(CERNLIB_HIGZ)
*
*             Create a view bank
      IF(ISWIT(7).EQ.0)CALL VIEWYZ(1)
#endif
      CALL UHINIT
*
 1000 FORMAT(/,' **** GEXAM4 VERSION 1.17.00 ( 10 May 1988 ) ',/)
*
      END
