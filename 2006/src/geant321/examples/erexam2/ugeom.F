*
* $Id: ugeom.F,v 1.1.1.1 1996/03/06 15:36:26 mclareni Exp $
*
* $Log: ugeom.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:26  mclareni
* Add geane321 examples
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      SUBROUTINE UGEOM
*
      DIMENSION PTBOX(3)
*
      DATA PTBOX/110.0,50.0,50.0/
*
      ISVOL=0
      IFIELD=3
      FIELDM=20.0
      TMAXFD=0.1
      DMAXMS=0.1
      DEEMAX=0.01
      EPSIL=0.01
      STMIN=0.01
      CALL GSTMED(1,'air$',15,
     +      ISVOL,IFIELD,FIELDM,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN,UBUF,0)
      CALL GSTMED(2,'iron$',10,
     +      ISVOL,IFIELD,FIELDM,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN,UBUF,0)
      CALL GSTMED(3,'alluminum$',9,
     +      ISVOL,IFIELD,FIELDM,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN,UBUF,0)
*
*     a big volume of air
      CALL GSVOLU('ABOX','BOX ',1,PTBOX,3,IVOLU)
*
*     an iron box surrunding the outer detector
      PTBOX(1) = 80.
      CALL GSVOLU('IBOX','BOX ',2,PTBOX,3,IVOLU)
*
*     divided in 8 parts
      CALL GSDVN('IDIV','IBOX', 8,1)
      CALL GSPOS('IBOX',1,'ABOX',20.,0.,0.,0,'ONLY')
*
*     an air box containing the chamber
      PTBOX(1) = 2.
      CALL GSVOLU('GBOX','BOX ',1,PTBOX,3,IVOLU)
      CALL GSPOS('GBOX',1,'IDIV',8.,0.,0.,0,'ONLY')
*
*     an alluminum box for the chamber walls
      PTBOX(1) = 1.
      CALL GSVOLU('DBOX','BOX ',3,PTBOX,3,IVOLU)
      CALL GSPOS('DBOX',1,'GBOX',0.,0.,0.,0,'ONLY')
*
*     an finally a "gas" box for the sensitive part of the detector
      PTBOX(1) = PTBOX(1) - .2
      PTBOX(2) = PTBOX(2) - .2
      PTBOX(3) = PTBOX(3) - .2
      CALL GSVOLU('WBOX','BOX ',1,PTBOX,3,IVOLU)
      CALL GSPOS('WBOX',1,'DBOX',0.,0.,0.,0,'ONLY')
*
      CALL GGCLOS
*
      END
