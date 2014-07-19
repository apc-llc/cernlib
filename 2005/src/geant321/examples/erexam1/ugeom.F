*
* $Id: ugeom.F,v 1.1.1.1 1996/03/06 15:36:25 mclareni Exp $
*
* $Log: ugeom.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:25  mclareni
* Add geane321 examples
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      SUBROUTINE UGEOM
*
* *** Define user geometry set up
*
#include "geant321/gconst.inc"
#include "geant321/gclist.inc"
#include "cugeom.inc"
*
      DIMENSION PAR( 5)
*
* *** Defines USER perticular materials
      CALL GSMATE( 1,'Vacuum  ',1.E-16,1.E-16,1.E-16,1.E+16,1.E+16,0,0)
      CALL GSMATE( 2,'Air     ', 14.61,7.3,0.001205,30423.,6750.,0,0)
      CALL GSMATE( 3,'Aluminium', 26.98,13.,2.7   ,8.90,37.2,0,0)
      CALL GSMATE( 4,'Iron     ', 55.85,26.,7.87  ,1.76,17.1,0,0)
      CALL GSMATE( 5,'Copper   ', 63.54,29.,8.96  ,1.43,14.8,0,0)
*
* *** Defines USER tracking media parameters
      FIELDM = 10.0
      TMAXFD = 10.0
      DMAXMS =  0.50
      DEEMAX =  0.05
      EPSIL  =  0.0100
      STMIN  =  0.0100
*
      IFIELD =  0
      CALL GSTMED( 1,'Fram'            , 2, 0 ,IFIELD, FIELDM,TMAXFD,
     *                DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
*
      CALL GSTMED( 2,'Muon detection'  , 2, 0 ,IFIELD, FIELDM,TMAXFD,
     *                DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
*
      CALL GSTMED( 3,'Solenoid'        , 3, 0 ,IFIELD, FIELDM,TMAXFD,
     *                DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
*
      IFIELD =  3
      CALL GSTMED( 4,'Beam            ', 1, 0 ,IFIELD,-FIELDM,TMAXFD,
     *                DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
*
      CALL GSTMED( 5,'Central Detector', 2, 0 ,IFIELD,-FIELDM,TMAXFD,
     *                DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
*
      CALL GSTMED( 6,'Electr. calorim.', 5, 0 ,IFIELD,-FIELDM,TMAXFD,
     *                DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
*
      CALL GSTMED( 7,'Hadronic calorim', 4, 0 ,IFIELD,+FIELDM,TMAXFD,
     *                DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
*
*
* *** Defines the GEOMETRY : typical detectors for collider ...
*
* *** Basic parameters
      TBEAM  = 5.
      TCDET  = 95.
      TECAL  = 40.
      TSOLN  = 10.
      THCAL  = 100.
      TMUON  = 50.
      DECAL  = 10.
      DHCAL  = 10.
*
* *** Absolute reference frame (box )
      TFRAM  = TBEAM + TCDET + TECAL + TSOLN + THCAL + TMUON
      PAR(1) = TFRAM
      PAR(2) = 50.
      PAR(3) = 50.
      IMED   = 1
      CALL GSVOLU('FRAM','BOX ',IMED ,PAR, 3 ,IVOLU)
*
* *** Beam
      PAR(1) = TBEAM
      PAR(2) = 50.
      PAR(3) = 50.
      IMED   = 4
      CALL GSVOLU('BEAM','BOX ',IMED ,PAR, 3 ,IVOLU)
*
      OGX   = 0.
      OGY   = 0.
      OGZ   = 0.
      CALL GSPOS ('BEAM', 1,'FRAM',OGX,OGY,OGZ, 0,'ONLY')
*
* *** Central Detector
      PAR(1) = 0.5*TCDET
      PAR(2) = 50.
      PAR(3) = 50.
      IMED   = 5
      CALL GSVOLU('CDET','BOX ',IMED ,PAR, 3 ,IVOLU)
*
      OGX   = TBEAM + 0.5*TCDET
      OGY   = 0.
      OGZ   = 0.
      CALL GSPOS ('CDET', 1,'FRAM',OGX,OGY,OGZ, 0,'ONLY')
*
* *** Electromagnetic calorimeter
      PAR(1) = 0.5*TECAL
      PAR(2) = 50.
      PAR(3) = 50.
      IMED   = 6
      CALL GSVOLU('ECAL','BOX ',IMED ,PAR, 3 ,IVOLU)
*
      OGX   = TBEAM + TCDET + 0.5*TECAL
      OGY   = 0.
      OGZ   = 0.
      CALL GSPOS ('ECAL', 1,'FRAM',OGX,OGY,OGZ, 0,'ONLY')
*
      CALL GSDVT ('DVEC','ECAL',DECAL,1,IMED,0)
*
* *** Solenoid
      PAR(1) = 0.5*TSOLN
      PAR(2) = 50.
      PAR(3) = 50.
      IMED   = 3
      CALL GSVOLU('SOLN','BOX ',IMED ,PAR, 3 ,IVOLU)
*
      OGX   = TBEAM + TCDET + TECAL + 0.5*TSOLN
      OGY   = 0.
      OGZ   = 0.
      CALL GSPOS ('SOLN', 1,'FRAM',OGX,OGY,OGZ, 0,'ONLY')
*
* *** Hadron calorimeter
      PAR(1) = 0.5*THCAL
      PAR(2) = 50.
      PAR(3) = 50.
      IMED   = 7
      CALL GSVOLU('HCAL','BOX ',IMED ,PAR, 3 ,IVOLU)
*
      OGX   = TBEAM + TCDET + TECAL + TSOLN + 0.5*THCAL
      OGY   = 0.
      OGZ   = 0.
      CALL GSPOS ('HCAL', 1,'FRAM',OGX,OGY,OGZ, 0,'ONLY')
*
      CALL GSDVT ('DVEH','HCAL',DHCAL,1,IMED,0)
*
* *** Muon chambers
      PAR(1) = 0.5*TMUON
      PAR(2) = 50.
      PAR(3) = 50.
      IMED   = 2
      CALL GSVOLU('MUON','BOX ',IMED ,PAR, 3 ,IVOLU)
*
      OGX   = TBEAM + TCDET + TECAL + TSOLN + THCAL + 0.5*TMUON
      OGY   = 0.
      OGZ   = 0.
      CALL GSPOS ('MUON', 1,'FRAM',OGX,OGY,OGZ, 0,'ONLY')
*
*     Optimisation of the search
      CALL GSORD ('FRAM' , 1)
*
* *** Close geometry banks. (obligatory system routine)
      CALL GGCLOS
*
      END
