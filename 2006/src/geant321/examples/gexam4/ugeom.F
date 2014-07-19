*
* $Id: ugeom.F,v 1.2 1996/02/01 15:00:38 ravndal Exp $
*
* $Log: ugeom.F,v $
* Revision 1.2  1996/02/01 15:00:38  ravndal
* Clean up repository
*
* Revision 1.1.1.1  1995/10/24 10:22:15  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UGEOM
*
************************************************************************
*                                                                      *
*             Routine to define the geometry of the set-up.            *
*                                                                      *
************************************************************************
*
#include "geant321/gconsp.inc"
#include "urgeom.inc"
*
      DIMENSION PAR(10),ABRASS(2),ZBRASS(2),WBRASS(2)
      DIMENSION AISOBU(3),ZISOBU(3),WGAS(3),ACO2(3),ZCO2(3)
      DIMENSION AURAN(2),ZURAN(2),WURAN(2)
      DATA AISOBU/12.01,1.01,39.95/
      DATA ZISOBU/6.,1.,18./
      DATA ACO2/12.01,16.01,39.95/
      DATA ZCO2/6.,8.,18./
      DATA ABRASS/63.54,65.37/
      DATA ZBRASS/29.,30./
      DATA WBRASS/.7,.3/
C
C             Uranium mixture
C
      DATA AURAN/235.,238./
      DATA ZURAN/92.,92./
      DATA WURAN/0.004,0.996/
*
*     -----------------------------------------------------------------
*
*
*             Defines materials
      CALL GSMATE( 1,'AIR$     ',  15.0,7.0,0.0012,30050.0,67500.0,0,0)
      CALL GSMATE( 2,'COPPER$  ', 63.54,29.,8.960 ,   1.43,   14.8,0,0)
      CALL GSMIXT( 3,'URANIUM$  ',AURAN,ZURAN,18.95 ,2,WURAN)
      CALL GSMATE( 4,'CARBON$  ', 12.01, 6., 2.265,  18.80,   49.9,0,0)
      CALL GSMIXT( 6,'BRASS$    ',ABRASS,ZBRASS,8.560   , 2,WBRASS)
*
      IF(IGAST.EQ.1)THEN
*
*             Argon/Isobuthane mixture (60% Ar and 40% Isobuthane)
*             First define Isobuthane compound and relative weights
*
         DISO   =0.00267
         DENS1  =0.002136
         WGAS(1)=4.
         WGAS(2)=10.
         CALL GSMIXT(15,'ISOBUTHAN$',AISOBU,ZISOBU,0.40*DISO,-2,WGAS)
         WGAS(1)=0.40*WGAS(1)
         WGAS(2)=0.40*WGAS(2)
         WGAS(3)=0.60
         CALL GSMIXT( 5,'ARG/ISOBU$',AISOBU,ZISOBU,DENS1, 3,WGAS)
      ELSE
*
*             Argon/CO2 mixture (10% Ar and 90% CO2)
*             First define CO2 compound and relative weights
*
         DENS2  =0.0019573
         DCO2   =0.001977
         WGAS(1)=1.
         WGAS(2)=2.
         CALL GSMIXT(15,'CO2$',ACO2,ZCO2,0.90*DCO2,-2,WGAS)
         WGAS(1)=0.90*WGAS(1)
         WGAS(2)=0.90*WGAS(2)
         WGAS(3)=0.10
         CALL GSMIXT( 5,'ARG/CO2$',ACO2,ZCO2,DENS2, 3,WGAS)
      ENDIF
*
*             Defines tracking media parameters.
      STEMAX =  BIG
      DEEMAX =  0.30
      EPSIL  =  0.005
      STMIN  =  0.1
*
      CALL GSTMED( 1, 'AIR$     ', 1, 0, 0, 0., 0., STEMAX,
     *            DEEMAX, EPSIL, STMIN, 0, 0)
*
      CALL GSTMED( 2, 'COPPER$  ', 2, 0, 0, 0., 0., STEMAX,
     *            DEEMAX, EPSIL, STMIN, 0, 0)
*
      CALL GSTMED( 3, 'URANIUM$ ', 3, 0, 0, 0., 0., STEMAX,
     *            DEEMAX, EPSIL, STMIN, 0, 0)
*
      CALL GSTMED( 4,'CARBON$   ', 4, 0, 0, 0., 0., STEMAX,
     *            DEEMAX, EPSIL, STMIN, 0, 0)
*
      CALL GSTMED( 5,'GAS$      ', 5, 0, 0, 0., 0., STEMAX,
     *            DEEMAX, EPSIL, STMIN, 0, 0)
*
      CALL GSTMED( 6,'BRASS$    ', 6, 0, 0, 0., 0., STEMAX,
     *            DEEMAX, EPSIL, STMIN, 0, 0)
*
      CALL GSTPAR(5,'STRA',1.)
      CALL GSTPAR(5,'DCUTE',5E-5)
*
*             Defines geometry of the set-up
*
*             Basic parameters
      UTHICK = 0.4
      CSHIEL = 0.1
      ESHIEL = 0.1
      WGS1   = 0.55
      WGS2   = 0.35
      BTHICK = 0.05
      AIRGP1 = 0.9
      CTHICK = 2.0
      AIRGP2 = 1.9
      EPCHAM = 1.6
      AIRGP3 = 1.7
      HMODL1 = (2.*CSHIEL+UTHICK+AIRGP1)*.5
      HSECT1 = HMODL1*64.
      HMODL2 = (2.*CSHIEL+UTHICK+AIRGP2)*.5
      HSECT2 = HMODL2*35.
      HMODL3 = (          CTHICK+AIRGP3)*.5
      HSECT3 = HMODL3*13.
      HLCALO = HSECT1+HSECT2+HSECT3+AIRGP1
*
*             Define the overall calorimeter
      PAR(1)=25.
      PAR(2)=25.
      PAR(3)=HLCALO
      CALL GSVOLU('CALO', 'BOX ', 1, PAR, 3, IVOLU)
*
*             Now define the three sections
      PAR(3)=HSECT1
      CALL GSVOLU('CAL1', 'BOX ', 1, PAR, 3, IVOLU)
      PAR(3)=HSECT2
      CALL GSVOLU('CAL2', 'BOX ', 1, PAR, 3, IVOLU)
      PAR(3)=HSECT3
      CALL GSVOLU('CAL3', 'BOX ', 1, PAR, 3, IVOLU)
*
*             and position them
      CALL GSPOS('CAL1',1,'CALO',0.0,0.0,
     * 0.5*AIRGP1-(HSECT2+HSECT3) ,0,'ONLY')
      CALL GSPOS('CAL2',1,'CALO',0.0,0.0,
     * 0.5*AIRGP1+HSECT1-HSECT3  ,0,'ONLY')
      CALL GSPOS('CAL3',1,'CALO',0.0,0.0,
     * 0.5*AIRGP1+HSECT1+HSECT2  ,0,'ONLY')
*
*             Now divide each section in modules
      CALL GSDVN('MOD1','CAL1',64,3)
      CALL GSDVN('MOD2','CAL2',35,3)
      CALL GSDVN('MOD3','CAL3',13,3)
*
*             Define copper shielding for type 1 and 2 modules
      PAR(3)=CSHIEL*.5
      CALL GSVOLU('SHIL', 'BOX ', 2, PAR, 3, IVOLU)
*
*             Define Uranium plate for type 1 and 2 modules
      PAR(3)=UTHICK*.5
      CALL GSVOLU('URPL', 'BOX ', 3, PAR, 3, IVOLU)
*
*             Define brass chamber for type 1 modules
      PAR(3)=(WGS1+BTHICK)*.5
      CALL GSVOLU('CHA1', 'BOX ', 6, PAR, 3, IVOLU)
      CALL GSDVN('TUB1','CHA1', 40, 2)
*
*             Define gas for chamber of type 1 modules
      PAR(1)=24.
      PAR(2)=1.2*.5
      PAR(3)=WGS1*.5
      CALL GSVOLU('GAS1', 'BOX ', 5, PAR, 3, IVOLU)
      CALL GSPOS('GAS1', 1, 'TUB1', 0.0, 0.0, 0.0, 0, 'ONLY')
*
*             Define epoxy wrapping for type 1 chambers
      PAR(1)=25.
      PAR(2)=25.
      PAR(3)=ESHIEL*.5
      CALL GSVOLU('EPO1', 'BOX ', 4, PAR, 3, IVOLU)
*
*             Define copper plate for type 3 modules
      PAR(3)=CTHICK*.5
      CALL GSVOLU('COPL', 'BOX ', 2, PAR, 3, IVOLU)
*
*             Define epoxy chamber for type 2 and 3 modules
      PAR(2)=47.*.5
      PAR(3)=EPCHAM*.5
      CALL GSVOLU('CHA2', 'BOX ', 4, PAR, 3, IVOLU)
      CALL GSDVN('TUB2', 'CHA2', 72, 2)
*
*             Define gas for chamber of type 2 and 3 modules
      PAR(1)=24.
      PAR(2)=(.65-.1)*.5
      PAR(3)=WGS2*.5
      CALL GSVOLU('GAS2', 'BOX ', 5, PAR, 3, IVOLU)
      CALL GSPOS('GAS2', 1, 'TUB2', 0.0, 0.0, 0.0, 0, 'ONLY')
*
*             Now position front chamber
      Z=-HLCALO+0.5*AIRGP1-(ESHIEL+WGS1+BTHICK)*0.5
      CALL GSPOS('EPO1',1,'CALO',0.0,0.0,Z,0,'ONLY')
      Z=-HLCALO+0.5*AIRGP1
      CALL GSPOS('CHA1',1,'CALO',0.0,0.0,Z,0,'ONLY')
      Z=-HLCALO+0.5*AIRGP1+(ESHIEL+WGS1+BTHICK)*0.5
      CALL GSPOS('EPO1',2,'CALO',0.0,0.0,Z,0,'ONLY')
*
*             Now assemble type 1 modules
      Z=CSHIEL*.5-HMODL1
      CALL GSPOS('SHIL',1,'MOD1',0.0,0.0,Z,0,'ONLY')
      Z=CSHIEL+UTHICK*.5-HMODL1
      CALL GSPOS('URPL',1,'MOD1',0.0,0.0,Z,0,'ONLY')
      Z=CSHIEL*1.5+UTHICK-HMODL1
      CALL GSPOS('SHIL',2,'MOD1',0.0,0.0,Z,0,'ONLY')
      Z=CSHIEL*2.+UTHICK+AIRGP1*.5-(WGS1+BTHICK+ESHIEL)*.5-HMODL1
      CALL GSPOS('EPO1',1,'MOD1',0.0,0.0,Z,0,'ONLY')
      Z=CSHIEL*2.+UTHICK+AIRGP1*.5-HMODL1
      CALL GSPOS('CHA1',1,'MOD1',0.0,0.0,Z,0,'ONLY')
      Z=CSHIEL*2.+UTHICK+AIRGP1*.5+(WGS1+BTHICK+ESHIEL)*.5-HMODL1
      CALL GSPOS('EPO1',2,'MOD1',0.0,0.0,Z,0,'ONLY')
*
*             Now assemble type 2 modules
      Z=CSHIEL*.5-HMODL2
      CALL GSPOS('SHIL',1,'MOD2',0.0,0.0,Z,0,'ONLY')
      Z=CSHIEL+UTHICK*.5-HMODL2
      CALL GSPOS('URPL',1,'MOD2',0.0,0.0,Z,0,'ONLY')
      Z=CSHIEL*1.5+UTHICK-HMODL2
      CALL GSPOS('SHIL',2,'MOD2',0.0,0.0,Z,0,'ONLY')
      Z=CSHIEL*2.+UTHICK+AIRGP2*.5-HMODL2
      CALL GSPOS('CHA2',1,'MOD2',0.0,0.0,Z,0,'ONLY')
*
*             Now assemble type 3 modules
      Z=CTHICK*.5-HMODL3
      CALL GSPOS('COPL',1,'MOD3',0.0,0.0,Z,0,'ONLY')
      Z=CTHICK+AIRGP3*.5-HMODL3
      CALL GSPOS('CHA2',1,'MOD3',0.0,0.0,Z,0,'ONLY')
*
*             Define geometry optimization
      CALL GSORD('CALO',3)
      CALL GSORD('MOD1',3)
      CALL GSORD('MOD2',3)
      CALL GSORD('MOD3',3)
*
*             Close geometry banks. Mandatory system routine.
*
      CALL GGCLOS
*
      END
