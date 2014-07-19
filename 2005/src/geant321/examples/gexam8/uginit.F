*
* $Id: uginit.F,v 1.2 1996/02/22 13:26:59 ravndal Exp $
*
* $Log: uginit.F,v $
* Revision 1.2  1996/02/22 13:26:59  ravndal
* Cleaning up CARTOCVS interface
*
* Revision 1.1.1.1  1995/10/24 10:22:22  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UGINIT
C.
C.    *
C.    *
C.    *        To initialise GEANT/USER  program and read data cards
C.    *
C.
#include "pvolum.inc"
#include "geant321/gcunit.inc"
#include "geant321/gclist.inc"
#include "geant321/gckine.inc"
 
C.
*
*             Open user files
*
      CALL UFILES
C.
C             Initialise GEANT
C
      CALL GINIT
      IMAT1=14
      IMAT2=25
      DZ1=0.3
      DZ2=0.6
      NZ=200
      IKINE=109
      PKINE(1)=10.
      PKINE(2)=0.
      PKINE(3)=0.
      CALL FFKEY('MATE',IMAT1,2,'INTEGER')
      CALL FFKEY('LAYE',DZ1,3,'MIXED')
      CALL GFFGO
      CLOSE(4)
      CALL GZINIT
      CALL GDINIT
      CALL GPART
C
C             Prints version number
C
      WRITE(LOUT,1000)
C
 
C              Geometry and materials description
 
      CALL UGEOM
C
      CALL GLOOK('MATE',LPRIN,NPRIN,IM)
      CALL GLOOK('TMED',LPRIN,NPRIN,IT)
      CALL GLOOK('VOLU',LPRIN,NPRIN,IV)
      IF(IM.NE.0)CALL GPRINT('MATE',0)
      IF(IT.NE.0)CALL GPRINT('TMED',0)
      IF(IV.NE.0)CALL GPRINT('VOLU',0)
 
C              Energy loss and cross-sections initialisations
 
      CALL GPHYSI
C
*             Create a view bank
      CALL VIEWYZ(1)
*
C             Define user histograms
 
      CALL UHINIT
 
 1000 FORMAT(/,'  GEXAM8 VERSION 1.00 ',/)
 
      END
