*
* $Id: uginit.F,v 1.3 1996/03/05 15:08:53 ravndal Exp $
*
* $Log: uginit.F,v $
* Revision 1.3  1996/03/05 15:08:53  ravndal
* Cleanup of CARTOCVS conversion
*
* Revision 1.2  1996/02/22 13:24:05  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:20  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UGINIT
C.    *
C.    *        To initialise GEANT/USER  program and read data cards
C.    *
#include "pvolum.inc"
#include "geant321/gcunit.inc"
#include "geant321/gclist.inc"
#include "geant321/gckine.inc"
#include "geant321/gcmulo.inc"
#include "celoss.inc"
#include "bootdt.inc"
#include "bootwk.inc"
*
*             Open user files
*
      CALL UFILES
C.
C             Initialise GEANT
C
      CALL GINIT
C *** volumes and bins definition
      NLTOT=20
      DLX0 = 1.
      NRTOT=20
      DRX0 = 0.25
      CALL FFKEY('BINS',NLTOT,4,'MIXED')
C *** material definition
      IMAT=21
      CALL FFKEY('MATE',IMAT,1,'INTEGER')
C *** define kinematic
      IKINE = 1
      PKINE(1)=10.
C *** bootstrap option
      JBOOT=0
      CALL FFKEY('BOOT',MEDIUM,23,'MIXED')
C *** read data cards
      CALL GFFGO
C *** achieve initialization
      NLTOT = MIN(NLTOT,NBIN)
      NRTOT = MIN(NRTOT,NBIN)
      CALL VZERO(SEL1,22*NBIN)
C
C
      CALL GZINIT
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
C
C              Energy loss and cross-sections initialisations
C
      CALL GPHYSI
C
#if defined(CERNLIB_CHECK)
      IF(IKINE.LE.3) THEN
         CALL GPLMAT(IMAT,1,'ALL',90,ELOW,0)
         CALL GPLMAT(IMAT,3,'ALL',90,ELOW,0)
       ELSE
         CALL GPLMAT(IMAT,IKINE,'ALL',90,ELOW,0)
      ENDIF
#endif
C
C             Define user histograms
C
      CALL UHINIT
C
 1000 FORMAT(/,'  GEXAM1 VERSION 2.05 ',/)
C
      END
