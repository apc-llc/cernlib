*
* $Id: uginit.F,v 1.4 1996/03/15 17:36:06 japost Exp $
*
* $Log: uginit.F,v $
* Revision 1.4  1996/03/15 17:36:06  japost
* Small Parallel code correction
*
* Revision 1.3  1996/03/15 17:23:01  japost
* Addition of parallel code
*
* Revision 1.2  1996/02/01 14:37:29  ravndal
* Clean up of the repository
*
* Revision 1.1.1.1  1995/10/24 10:22:09  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE UGINIT
C.
C.    *
C.    *
C.    *        To initialise GEANT/USER  program and read data cards
C.    *
C.
#include "geant321/gcunit.inc"
#include "geant321/gclist.inc"
#include "geant321/gckine.inc"
#include "pvolum.inc"
#include "celoss.inc"
 
#if  defined(CERNLIB_PARA)
#if defined(MEIKO)
c     On the Meiko CS-2 reading from stdin-5 from multiple nodes
c       fails to give the same input: instead node 1 gets line 1, 
c       node 2 gets line 2, and so on
c
c     Reset the FFREAD Input unit to the one in ufiles ...
c
      call ffset('LINP', 4 )
c          -----------------
#endif
#endif
C.
*
*             Open user files
*
      CALL UFILES
C.
C             Initialise GEANT
C
      CALL GINIT
      IMAT=10
      PKINE(1)=10.
      PKINE(2)=1.
      EINTOT=0.
      CALL VZERO(SEL1,640)
      NL=20
      NR=20
      CALL FFKEY('BINS',NL,2,'INTEGER')
      CALL FFKEY('MATE',IMAT,1,'INTEGER')
      CALL GFFGO
      CALL GZINIT
      CALL GPART
C
C             Prints version number
C
      WRITE(LOUT,10000)
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
 
C             Define user histograms
 
      CALL UHINIT
 
10000 FORMAT(/,'  GEXAM1 VERSION 1.00 ',/)
 
      END
