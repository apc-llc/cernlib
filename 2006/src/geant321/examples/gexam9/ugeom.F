*
* $Id: ugeom.F,v 1.2 1996/02/22 13:32:43 ravndal Exp $
*
* $Log: ugeom.F,v $
* Revision 1.2  1996/02/22 13:32:43  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:25  cernlib
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
#include "urgeom.inc"
#include "hsizes.inc"
#include "tstcom.inc"
*
      DIMENSION AF(3),ZF(3),WMATF(3)
      DIMENSION PAR(10),ABRASS(2),ZBRASS(2),WBRASS(2)
      DIMENSION AISOBU(3),ZISOBU(3),WGAS(3),ACO2(3),ZCO2(3)
*     FRONT RADIUS OF MODUL:
      DATA RIHB/88.6/,RIEC/25.0/
      DATA ECALGH/0./
*     MODUL SIZE:
      DATA WMOD/52.3/,HMOD/86.9/,EMOD/60./
*
*            Define parameters (= half sizes of detectors)
*            ( convention: X1... half x at Z min.
*                          X2... half x at Z max.)
*
      DATA DISTCA/20. /,TOLERA/ 5. /
      DATA MATE,TMED,VOLU/4HMATE,4HTMED,4HVOLU/
      DATA AISOBU/12.01,1.01,39.95/
      DATA ZISOBU/6.,1.,18./
      DATA ACO2/12.01,16.01,39.95/
      DATA ZCO2/6.,8.,18./
      DATA ABRASS/63.54,65.37/
      DATA ZBRASS/29.,30./
      DATA WBRASS/.7,.3/
*
*             Iron+Nickel+Crome  compound parameters
      DATA AF/55.847,58.71,51.998/
      DATA ZF/26.,28.,24./
      DATA WMATF/0.703964,0.099,0.197/
*
      X1CALO = 25.0
      X2CALO = 32.86
      YCALO  = 23.85
      X1HCAL = 16.68
      ZHCAL  = 41.85
      X2HIRO = 17.99
      ZHIRO  =  3.57
      CHADEP =  0.82
*
      X1ECAL = 10.5
      ZCALO  = ZHCAL + (DISTCA + ECALGH + TOLERA)*.5
      X2HCAL = X2CALO
      X1HIRO = X1HCAL
      X1HURA = X2HIRO
      X2HURA = X2CALO
      ZHURA  = ZHCAL - ZHIRO
*
*             Defines materials
      CALL GSMATE( 1,'AIR$     ',  15.0,7.0,0.0012,30050.0,67500.0,0,0)
      CALL GSMATE( 2,'COPPER$  ', 63.54,29.,8.960 ,   1.43,   14.8,0,0)
      CALL GSMATE( 3,'URANIUM$ ',238.03,92.,18.950,   0.32,   12.0,0,0)
      CALL GSMIXT( 6,'BRASS$    ',ABRASS,ZBRASS,8.560   , 2,WBRASS)
      CALL GSMIXT( 7,'IRON(COMPOUND)$',AF,ZF,7.8,3,WMATF)
*
*             Argon/CO2 mixture (90% Ar and 10% CO2)
*             First define CO2 compound and relative weights
*
      DENS2  =0.0019573
      DCO2   =0.001977
      WGAS(1)=1.
      WGAS(2)=2.
      CALL GSMIXT(15,'CO2$',ACO2,ZCO2,0.90*DCO2,-2,WGAS)
      WGAS(1)=0.10*WGAS(1)
      WGAS(2)=0.10*WGAS(2)
      WGAS(3)=0.90
      CALL GSMIXT( 5,'ARG/CO2$',ACO2,ZCO2,DENS2, 3,WGAS)
*
*             Defines tracking media parameters.
      DMAXMS =  0.50
      DEEMAX =  0.20
      EPSIL  =  0.01
      STMIN  =  0.10
*
      CALL GSTMED( 1, 'AIR$     ', 1, 0, 0, 0., 0., DMAXMS,
     +            DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED( 2, 'COPPER$  ', 2, 0, 0, 0., 0., DMAXMS,
     +            DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED( 3, 'URANIUM$ ', 3, 0, 0, 0., 0., DMAXMS,
     +            DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED( 5,'GAS$      ', 5, 1, 0, 0., 0., DMAXMS,
     +            DEEMAX, 0.001, 0.1, 0, 0)
      CALL GSTMED( 6,'BRASS$    ', 6, 0, 0, 0., 0., DMAXMS,
     +            DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED( 7,'IRON$     ', 7, 0, 0, 0., 0., DMAXMS,
     +            DEEMAX, EPSIL, STMIN, 0, 0)
      FIELDM = 5.
      IFIELD = 0
      TMAXFD = 0.01
      DMAXMS = 0.90
      DEEMAX = 0.10
      EPSIL = 0.001
      STMIN = 0.10
      EPSILM = 0.001
      STMINM = 0.01
*
*           TEST SET UP
      TPI16=TAN(3.141593/16.)
*-------- DEFINE A TUBS FOR BGO + HCAL TEST
      PAR(1)=TPI16*RIEC*1.5
      PAR(2)=TPI16*(RIHB+HMOD)*1.5
      PAR(3)=0.5*WMOD*1.5
      PAR(4)=0.5*(HMOD+EMOD)
      CALL GSVOLU('BHTS','TRD1',1,PAR,4,IVOL)
*
      PAR(1)=TPI16*RIHB
*-------- DEFINE A MODUL(STEEL):HBMC
      PAR(2)=TPI16*(RIHB+HMOD)
      PAR(3)=0.5*WMOD
      PAR(4)=0.5*HMOD
      CALL GSVOLU('HBMC','TRD1',7,PAR,4,IVOL)
      CALL GSROTM (100, 90., 0., 90., 90., 0., 0.)
*
      XC=0.0
      YC=0.0
      ZC=0.5*EMOD
      CALL GSPOS('HBMC',1,'BHTS',XC,YC,ZC,100,'ONLY')
*
      CALL HCATST
*
*             Close geometry banks. Mandatory system routine.
      CALL GGCLOS
*                                                   end of UGEOM
      END
