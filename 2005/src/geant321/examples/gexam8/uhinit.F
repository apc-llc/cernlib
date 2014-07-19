*
* $Id: uhinit.F,v 1.2 1996/03/05 15:10:21 ravndal Exp $
*
* $Log: uhinit.F,v $
* Revision 1.2  1996/03/05 15:10:21  ravndal
* Cleanup CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:22  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UHINIT
C
C     *       To book the user's histograms
C     *
 
#include "geant321/gcbank.inc"
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "pvolum.inc"
      CHARACTER*8 CHNAME(10)
      CHARACTER*8 CHNAM2(12)
      DATA CHNAME/'IEVENT  ','IPART   ','GEKIN   ','ISTOP   ','INWVOL  '
     +           ,'STEP    ','DESTEP  ','DXMIP   ','TOFG    ','IPL     '
     +           /
      DATA CHNAM2/'IEVENT  ','IPART   ','GEKIN   ','ISTOP   ','INWVOL  '
     +           ,'STEP    ','DESTEP  ','ENER    ','DEDX0   ','DEDX1   '
     +           ,'DEDX2','DEDX3'/
 
C
C ***            Histograms for shower development
C
      JMA1=LQ(JMATE-IMAT1)
      JMA2=LQ(JMATE-IMAT2)
      DENS1=Q(JMA1+8)
      DENS2=Q(JMA2+8)
      ZMAX=ZECAL
      RMAX=25.
      EMAX=2.*PKINE(1)
      SMAX=1.5*EMAX*DZ2*DENS2/(DZ1*DENS1)
C
      CALL HBOOK1(11,'Total energy deposition by type 1'
     *,100, 0.,EMAX, 0.0)
      CALL HBOOK1(12,'Total energy deposition by type 2'
     *,100, 0.,EMAX, 0.0)
      CALL HBOOK1(13,'Total energy deposition by type 3'
     *,100, 0.,EMAX, 0.0)
      CALL HBOOK1(14,'Total energy deposition by type 4'
     *,100, 0.,EMAX, 0.0)
      CALL HBOOK1(15,'Total energy deposition by type 5'
     *,100, 0.,EMAX, 0.0)
*
      CALL HBOOK1(21,'Total energy seen in MEDI by type 1'
     *,100, 0.,SMAX, 0.0)
      CALL HBOOK1(22,'Total energy seen in MEDI by type 2'
     *,100, 0.,SMAX, 0.0)
      CALL HBOOK1(23,'Total energy seen in MEDI by type 3'
     *,100, 0.,SMAX, 0.0)
      CALL HBOOK1(24,'Total energy seen in MEDI by type 4'
     *,100, 0.,SMAX, 0.0)
      CALL HBOOK1(25,'Total energy seen in MEDI by type 5'
     *,100, 0.,SMAX, 0.0)
*
      CALL HBOOK1(31,'Longit energy deposition by type 1'
     *, 100,-ZMAX,ZMAX,0.)
      CALL HBOOK1(32,'Longit energy deposition by type 2'
     *, 100,-ZMAX,ZMAX,0.)
      CALL HBOOK1(33,'Longit energy deposition by type 3'
     *, 100,-ZMAX,ZMAX,0.)
      CALL HBOOK1(34,'Longit energy deposition by type 4'
     *, 100,-ZMAX,ZMAX,0.)
      CALL HBOOK1(35,'Longit energy deposition by type 5'
     *, 100,-ZMAX,ZMAX,0.)
*
      CALL HBOOK1(41,'Lateral energy deposition by type 1'
     *, 100,0.,RMAX,0.)
      CALL HBOOK1(42,'Lateral energy deposition by type 2'
     *, 100,0.,RMAX,0.)
      CALL HBOOK1(43,'Lateral energy deposition by type 3'
     *, 100,0.,RMAX,0.)
      CALL HBOOK1(44,'Lateral energy deposition by type 4'
     *, 100,0.,RMAX,0.)
      CALL HBOOK1(45,'Lateral energy deposition by type 5'
     *, 100,0.,RMAX,0.)
*
      CALL HBOOK1(51,'Muon range',100,ZECAL,2.*ZECAL,0.)
*
      CALL HBOOK1(65,'Muon DESTEP in MEDI',100,0.,0.,0.)
*
      CALL HROPEN(61,'NTUPLE','gexam8.hist','N',1024,ISTAT)
      IF(ISWIT(8).NE.0)THEN
         CALL HBOOKN(1,'NTUPLE',10,'//NTUPLE',1000,CHNAME)
         CALL HBOOKN(2,'NTUPLE',12,'//NTUPLE',1000,CHNAM2)
      ENDIF
 
      END
