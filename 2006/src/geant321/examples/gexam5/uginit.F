*
* $Id: uginit.F,v 1.2 1996/02/01 15:03:17 ravndal Exp $
*
* $Log: uginit.F,v $
* Revision 1.2  1996/02/01 15:03:17  ravndal
* Clean up repository
*
* Revision 1.1.1.1  1995/10/24 10:22:17  cernlib
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
*              To initialise GEANT3 program and read data cards        *
*                                                                      *
************************************************************************
*
#include "geant321/gckine.inc"
#include "geant321/gcunit.inc"
#include "nascom.inc"
*
*     -----------------------------------------------------------------
*
*             Open user files
*
      CALL UFILES
*
*             Initialize GEANT
C..geant..
      CALL GINIT
*
*             Prints version number
*
      WRITE(LOUT,1000)
*
*             IKINE  = particle type (default=1=gamma)
*             PKINE(1)=particle energy
*             PKINE(2)=vertex position in Z
*             IKINE and PKINE can be changed with the data card KINE
*
      NTWO =0
      IKINE=1
      PKINE(1)=0.00443
      PKINE(2)=-79.24
*
*             Read data cards with FFREAD
*
C..geant..
      CALL GFFGO
*
*             Initialize GEANT/ZBOOK data structures
*
C..geant..
      CALL GZINIT
#if defined(CERNLIB_HIGZ)
*
*             Initialize graphics package
*
      CALL GDINIT
#endif
*
*             Geometry and materials description.
*
      CALL UGEOM
*
*             Particle table definition and energy loss initialization.
*
C..geant..
      CALL GPART
C..geant..
      CALL GPHYSI
#if defined(CERNLIB_HIGZ)
*
*             Initialize COMPTEL graphics
*
      CALL VIEWYZ(1)
#endif
C..book user histograms:
      CALL UHINIT
*
 1000 FORMAT(/,'  GEXAM5 VERSION 1.00 : ',/)
      END
