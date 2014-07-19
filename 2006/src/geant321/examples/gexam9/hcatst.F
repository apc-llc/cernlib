*
* $Id: hcatst.F,v 1.2 1996/02/22 13:32:41 ravndal Exp $
*
* $Log: hcatst.F,v $
* Revision 1.2  1996/02/22 13:32:41  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:24  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE HCATST
****************************************************************
*
*     Defines a module of L3 Hadron calorimeter
*
****************************************************************
#include "urgeom.inc"
      DIMENSION TWALL(7),PAR(4),NLFI(30)
      CHARACTER*4  NAMEH(1),NAMED(2)
      INTEGER  NBITH(1),NBITD(2)
      REAL  FACTH(1), ORIGH(1)
      DATA  NAMEH /'EDEP'/,NBITH /32/,FACTH /1.E+07/,ORIGH /0./
      DATA  NAMED /'IDNT','EDEP'/,NBITD /20,16/
*
****** THE RELEVANT PARAMETERS ARE :
*     FRONT RADIUS OF MODUL:
      DATA RIHB/88.6/
*     MODUL SIZE:
      DATA HMOD/86.9/
*     MODUL WIDTH:
      DATA WMOD/52.3/
*---- TWALL(1)-THICKNESS OF MODUL FRONT WALL
*          (2)-//---------- BACK WALL
*          (3)-//-----//- SIDE WALL IN F-PROJECTION (Phi)
*          (4)-//-----//-------//-  IN Z-PROJECTION (Zeta)
*          (5)-GAP BETWEEN SIDE WALL AND CHAMBER IN Z-PROJECTION
*          (6)-//-----//-----//-----//-----//----//-F-PROJECTION
*          (7)-//-----//-----//----- AND U-PLATE IN Z-PROJECTION
      DATA TWALL/3.9,1.5,0.4,1.5,0.8,0.5,0.4/
*---- AUXILIARY VOLUME SIZE(FIRST F/Z-CHAMBERS+STEEL PLATE):
      DATA TBPL/3.3/
*---- STEEL PLATE THICKNESS:
      DATA TFEA/1.5/
*---- ENDPIECE THICKNESS:
      DATA TENDP/0.7/
*---- CHAMBER PITCH:
      DATA PITCH/1.06/
*---- CHAMBER THICKNESS:
      DATA TCHAM/0.56/
*---- WIDTH/THICKNESS OF GAS CELL:
      DATA WCELL/1.0/,TCELL/0.5/
*---- URANIUM SHIELD THICKNESS(COPPER):
      DATA TSHL/0.07/
*---- ABSORBER THICKNESS(URANIUM):
      DATA TABS/0.5/
*---- SLOT THICKNESS FOR CHAMBERS(AIR):
      DATA TGAP/0.84/
*---- NUMBER OF TUBES IN ONE LAYER OF Z-PROJECTION:
      DATA NLZE/45/
*---- NUMBER OF TUBES IN EACH LAYER OF F-PROJECTION:
      DATA NLFI/33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,
     +49,50,51,52,53,54,55,56,57,58,58,58,58,58/
*
      DATA ITAIR /0/, ITCU /0/, ITFE /0/, ITGAC /0/, ITU /0/
*
*---------------------------------------------------------------
      TPI16=TAN(3.141593/16.)
      TGCH=TABS+TGAP
*      HLCALO=0.5*HMOD
*------- DEFINE VOLUME 1 :HV1C(AIR) ---------
      DZUSE=0.5*WMOD-TWALL(4)
      RCM=RIHB+0.5*HMOD
      R1=RIHB+TWALL(1)
      R2=R1+TBPL+48*TGCH
      DX1=TPI16*R1-TWALL(3)
      DX2=TPI16*R2-TWALL(3)
      PAR(1)=DX1
      PAR(2)=DX2
      PAR(3)=DZUSE
      PAR(4)=0.5*(R2-R1)
      XC=0.
      YC=0.
      ZC=-(RCM-0.5*(R1+R2))
      CALL GSVOLU('HV1C','TRD1',1,PAR,4,IVOL)
      CALL GSPOS('HV1C',1,'HBMC',XC,YC,ZC,0,'ONLY')
*
*------- DEFINE VOLUME 2 :HV2C(AIR) ---------
      R3=RIHB+HMOD-TWALL(2)
      PAR(1)=DX2
      PAR(2)=DZUSE
      PAR(3)=(R3-R2)*0.5
      ZC=0.5*(R2+R3)-RCM
      CALL GSVOLU('HV2C','BOX ',1,PAR,3,IVOL)
      CALL GSPOS('HV2C',1,'HBMC',XC,YC,ZC,0,'ONLY')
*
******** DEFINE FI-CHAMB,ZETA-CHAMB,U-PLATE:HVFC,HVZC,HBUC *********
*
      PAR(1)=0.
      PAR(2)=0.
      PAR(3)=0.
      CALL GSVOLU('HVFC','BOX ',6,PAR,0,IVOL)
      CALL GSVOLU('HVZC','BOX ',6,PAR,0,IVOL)
      CALL GSVOLU('HBUC','BOX ',3,PAR,0,IVOL)
      TAIR=0.5*(TGAP-TCHAM-2.*TSHL)
*
*------- POSITION (2-25)F-CHAM,(2-25)T-CHAM AND U IN HV1C --------
      DO 10  K=1,24
*------------ F-CHAMBERS --------------
         PAR(1)=NLFI(K+1)*PITCH*0.5+0.000001
         PAR(2)=DZUSE-TWALL(5)-TENDP
         PAR(3)=0.5*(TCHAM+2.*TSHL)
         STEP=2.*(TABS+TGAP)
         XC=0.
         YC=0.
         RIF=R1+TBPL+TABS+0.5*TGAP+(K-1)*STEP
         ZCF=RIF-0.5*(R1+R2)+TAIR
         CALL GSPOSP('HVFC',2*K+1,'HV1C',XC,YC,ZCF,0,'ONLY',PAR,3)
*
*------------ T-CHAMBERS --------------
         RIT=R1+TBPL+2.*TGCH-0.5*TGAP+(K-1)*STEP
         PAR(1)=TPI16*RIT-TWALL(3)-TWALL(6)-TENDP
         PAR(2)=NLZE*PITCH*0.5+0.000001
         PAR(3)=0.5*(TCHAM+2.*TSHL)
         ZCT=RIT-0.5*(R1+R2)+TAIR
         CALL GSPOSP('HVZC',2*K+2,'HV1C',XC,YC,ZCT,0,'ONLY',PAR,3)
*
*---------------- U-PLATES ---------------
         RIU1=R1+TBPL+0.5*TABS+(K-1)*STEP
         RIU2=RIU1+TGAP+TABS
         PAR(1)=TPI16*RIU1-TWALL(3)-TWALL(6)
         PAR(2)=DZUSE-TWALL(7)
         PAR(3)=0.5*TABS
         XC=0.
         YC=0.
         ZC1=RIU1-0.5*(R1+R2)
         CALL GSPOSP('HBUC',2*K-1,'HV1C',XC,YC,ZC1,0,'ONLY',PAR,3)
         PAR(1)=TPI16*RIU2-TWALL(3)-TWALL(6)
         ZC2=RIU2-0.5*(R1+R2)
         CALL GSPOSP('HBUC',2*K,'HV1C',XC,YC,ZC2,0,'ONLY',PAR,3)
   10 CONTINUE
*
*------ POSITION (26-30)F-CHAM,(26-30)T-CHAM AND U IN HV2C -------
      DO 20  K=1,5
*------------- F-CHAMBERS -------------
         PAR(1)=NLFI(25+K)*PITCH*0.5+0.000001
         PAR(2)=DZUSE-TWALL(5)-TENDP
         PAR(3)=0.5*(TCHAM+2.*TSHL)
         XC=0.
         YC=0.
         ZCF=-0.5*(R3-R2)+TABS+0.5*TGAP+(K-1)*STEP+TAIR
         CALL GSPOSP('HVFC',2*K+49,'HV2C',XC,YC,ZCF,0,'ONLY',PAR,3)
*------------- T-CHAMBERS -------------
         PAR(1)=DX2-TWALL(6)-TENDP
         PAR(2)=NLZE*PITCH*0.5+0.000001
         PAR(3)=0.5*(TCHAM+2.*TSHL)
         ZCT=ZCF+TGAP+TABS
         CALL GSPOSP('HVZC',2*K+50,'HV2C',XC,YC,ZCT,0,'ONLY',PAR,3)
*------------- U-PLATES ---------------
         PAR(1)=DX2-TWALL(6)
         PAR(2)=DZUSE-TWALL(7)
         PAR(3)=0.5*TABS
         XC=0.
         YX=0.
         ZCU1=-0.5*(R3-R2)+0.5*TABS+(K-1)*STEP
         ZCU2=ZCU1+TGAP+TABS
         CALL GSPOSP('HBUC',2*K-1,'HV2C',XC,YC,ZCU1,0,'ONLY',PAR,3)
         CALL GSPOSP('HBUC',2*K,'HV2C',XC,YC,ZCU2,0,'ONLY',PAR,3)
   20 CONTINUE
*
*------ POSITION FIRST F/T-CHAMBERS IN HV1C ---------
*
      PAR(1)=NLFI(1)*PITCH*0.5+0.000001
      PAR(2)=DZUSE-TWALL(5)-TENDP
      PAR(3)=0.5*(TCHAM+2.*TSHL)
      XC=0.
      YC=0.
      RT1=R1+TBPL-0.5*TGAP
      ZCT=RT1-0.5*(R1+R2)
      ZCF=ZCT-TGAP-TFEA
      CALL GSPOSP('HVFC',1,'HV1C',XC,YC,ZCF,0,'ONLY',PAR,3)
      PAR(1)=TPI16*RT1-TWALL(3)-TWALL(6)-TENDP
      PAR(2)=NLZE*PITCH*0.5+0.000001
      CALL GSPOSP('HVZC',2,'HV1C',XC,YC,ZCT,0,'ONLY',PAR,3)
*
*------ POSITION FE-ABSORBER IN HV1C --------
*
      PAR(1)=TPI16*(R1+TBPL-TGAP-TFEA) -TWALL(3)
      PAR(2)=DZUSE-TWALL(5)
      PAR(3)=0.5*TFEA
      CALL GSVOLU('HBIC','BOX ',7,PAR,3,IVOL)
      XC=0.
      YC=0.
      ZC=ZCF+0.5*TGAP+0.5*TFEA
      CALL GSPOS('HBIC',1,'HV1C',XC,YC,ZC,0,'ONLY')
*
*------ DEFINE SUBDIVISION IN FI/ZETA DIRECTIONS:HWFC,HWZC -----
      CALL GSDVT('HWFC','HVFC',PITCH,1, 0, 58)
      CALL GSDVT('HWZC','HVZC',PITCH,2, 0, 45)
*------ DEFINE ELEM. GAS CELL:HBFC,HBZC ------------------------
      PAR(1)=0.5*WCELL
      PAR(2)=-1.
      PAR(3)=0.5*TCELL
      CALL GSVOLU('HBFC','BOX ',5,PAR,3,IVOL)
      PAR(1)=-1.
      PAR(2)=0.5*WCELL
      CALL GSVOLU('HBZC','BOX ',5,PAR,3,IVOL)
*------ POSITION IT IN ALL FI/ZETA CHAMBERS --------------------
      XC=0.
      YC=0.
      ZC=0.
      CALL GSPOS('HBFC',1,'HWFC',XC,YC,ZC,0,'ONLY')
      CALL GSPOS('HBZC',1,'HWZC',XC,YC,ZC,0,'ONLY')
#if defined(CERNLIB_VSOP)
*--------- FORCE THE ORDERING SEARCH IN HV1C,HV2C --------------
      CALL GSORD('HBM*',3)
      CALL GSORD('HV1C',3)
      CALL GSORD('HV2C',3)
#endif
      CALL GSDETV ('HTST', 'HBFC', 1, 500, 0, ISET, IDET)
      CALL GSDETV ('HTST', 'HBZC', 1, 500, 0, ISET, IDET)
      CALL GSDETH ('HTST', 'HBFC', 1, 'EDEP', 32, 0., 1.E+07)
      CALL GSDETH ('HTST', 'HBZC', 1, 'EDEP', 32, 0., 1.E+07)
*                                                             END HCATST
      END
