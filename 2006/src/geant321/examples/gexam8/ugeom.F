*
* $Id: ugeom.F,v 1.2 1996/03/05 15:09:55 ravndal Exp $
*
* $Log: ugeom.F,v $
* Revision 1.2  1996/03/05 15:09:55  ravndal
* Cleanup CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:22  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UGEOM
C
C ***          Define user geometry set up
C
 
#include "geant321/gcbank.inc"
#include "pvolum.inc"
 
      DIMENSION PAR( 8)
      DIMENSION ZLG(6),ALG(6),WLG(6)
      DIMENSION A(3),Z(3),WMAT(3)
      DIMENSION AF(3),ZF(3),WMATF(3)
      DIMENSION AURAN(2),ZURAN(2),WURAN(2)
      DIMENSION AWATER(2),ZWATER(2),WWATER(2)
      DIMENSION AISOBU(3),ZISOBU(3),WGAS(3)
      DIMENSION ASCI(2),ZSCI(2),WSCI(2)
C
C             Argon/Isobuthane compound
C
      DATA AISOBU/12.01,1.01,39.95/
      DATA ZISOBU/6.,1.,18./
C
C             Water compound parameters
C
      DATA AWATER/1.,16./
      DATA ZWATER/1.,8./
      DATA WWATER/2.,1./
C
C             Lead glass mixture parameters
C
      DATA ZLG/  82.00,  19.00,  14.00,  11.00,  8.00,  33.00/
      DATA ALG/ 207.19,  39.102,  28.088,  22.99, 15.999,  74.922/
      DATA WLG/ .65994, .00799, .126676, .0040073,.199281, .00200485/
C
C             BGO compound parameters
C
      DATA A/208.98,72.59,15.999/
      DATA Z/83.,32.,8./
      DATA WMAT/4.,3.,12./
C
C             Iron+Nickel+Crome  compound parameters
C
      DATA AF/55.847,58.71,51.998/
      DATA ZF/26.,28.,24./
      DATA WMATF/0.703964,0.099,0.197/
      DATA ALAR,ZLAR,WLAR,DLAR,NLAR/40.,18.,1.,1.40,-1/
C
C             Uranium mixture
C
      DATA AURAN/235.,238./
      DATA ZURAN/92.,92./
      DATA WURAN/0.004,0.996/
C
C             Scintillator
C
      DATA ASCI/12.,1./
      DATA ZSCI/6.,1./
      DATA WSCI/1.,1./
      DATA DSCI/1.032/
C
C ***          Defines USER particular materials
C
*
*             Argon/Isobuthane mixture (60% Ar and 40% Isobuthane)
*             First define Isobuthane compound and relative weights
*
      DISO   =0.00267
      DENS1  =0.002136
      WGAS(1)=4.
      WGAS(2)=10.
      CALL GSMIXT(4,'ISOBUTHAN$',AISOBU,ZISOBU,0.40*DISO,-2,WGAS)
      WGAS(1)=0.40*WGAS(1)
      WGAS(2)=0.40*WGAS(2)
      WGAS(3)=0.60
      CALL GSMIXT( 5,'ARG/ISOBU$',AISOBU,ZISOBU,DENS1, 3,WGAS)
      CALL GSMATE( 9,'ALUMINIUM$', 26.98,13.,2.7   , 8.9,37.2,0,0)
      CALL GSMATE(11,'COPPER$   ', 63.54,29.,8.96  ,1.43,14.8,0,0)
      CALL GSMATE(12,'LEAD$     ',207.19,82.,11.35 ,0.56,18.5,0,0)
      CALL GSMATE(13,'LEAD$     ',207.19,82.,11.35 ,0.56,18.5,0,0)
*      CALL GSMATE(14,'URANIUM$  ',238.03,92.,18.95 ,0.32,12. ,0,0)
      CALL GSMIXT(14,'URANIUM$  ',AURAN,ZURAN,18.95 ,2,WURAN)
      CALL GSMATE(15,'AIR$      ', 14.61,7.3,0.001205,30423.,6750.,0,0)
      CALL GSMATE(16,'VACUUM$ ',1.E-16,1.E-16,1.E-16,1.E+16,1.E+16,0,0)
      CALL GSMIXT(10,'IRON(COMPOUND)$',AF,ZF,7.8,3,WMATF)
      CALL GSMIXT(21,'BGO(COMPOUND)$',A,Z,7.1,-3,WMAT)
      CALL GSMIXT(22,'LEAD GLASS$',ALG,ZLG,5.2,6,WLG)
      CALL GSMATE(23,'PLAST SC$',  6.25,3.4,1.032 ,   43.0,   437.,0,0)
      CALL GSMIXT(24,'SCINTILLATOR$',ASCI,ZSCI,DSCI,-2,WSCI)
      CALL GSMIXT(25,'liq. Argon$',ALAR,ZLAR,DLAR,NLAR,WLAR)
      CALL GSMIXT(26,'Water$$',AWATER,ZWATER,1.,-2,WWATER)
      CALL GSMATE(32,'LEADLEGER$',207.19,82.,6. ,0.56,18.5,0,0)
      CALL GSMATE(33,'LEADLEGER$',207.19,82.,6. ,0.56,18.5,0,0)
C
C ***          Defines USER tracking media parameters
C
C
 
      FIELDM =  0.
      IFIELD =  0
      TMAXFD =  10.
      DMAXMS =  1.
      DEEMAX =  0.1
      EPSIL  =  0.001
      STMIN  = -0.01
 
      CALL GSTMED( 1,'ABSORBER$'              ,IMAT1, 0 , IFIELD,
     *                FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
      CALL GSTMED( 2,'ACTIVE MEDIUM$'         ,IMAT2, 0 , IFIELD,
     *                FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
      IF(IMAT2.EQ.24)THEN
         CALL GSTPAR(2,'BIRK1',1.)
         CALL GSTPAR(2,'BIRK2',0.013)
         CALL GSTPAR(2,'BIRK3',9.6E-6)
      ENDIF
 
 
C
C ***          Defines USER'S VOLUMES
C
      JMA    = LQ(JMATE-IMAT1)
      X1     = Q(JMA+9)
      ABS1   = Q(JMA+10)
      JMA    = LQ(JMATE-IMAT2)
      X2     = Q(JMA+9)
      NMED1  = 1
      NMED2  = 2
      DZ=DZ1+DZ2
      ZECAL=NZ*0.5*DZ
      RECAL=ZECAL
 
      PAR(1) = 0.
      PAR(2) = RECAL
      PAR(3) = ZECAL
      CALL GSVOLU( 'ECAL' , 'TUBE' ,1, PAR , 3 , IVOL )
      CALL GSDVN('BLOC','ECAL',NZ,3)
      PAR(1) = 0.
      PAR(2) = RECAL
      PAR(3) = 0.5*DZ2
      CALL GSVOLU( 'MEDI' , 'TUBE' ,2, PAR , 3 , IVOL )
      ZC=0.5*DZ-PAR(3)
      CALL GSPOS( 'MEDI' ,1, 'BLOC' , 0.   , 0.   , ZC  , 0,'ONLY')
 
C ***          Close geometry banks. (obligatory system routine)
 
      CALL GGCLOS
 
      END
