*
* $Id: ugeom.F,v 1.2 1996/02/01 14:37:28 ravndal Exp $
*
* $Log: ugeom.F,v $
* Revision 1.2  1996/02/01 14:37:28  ravndal
* Clean up of the repository
*
* Revision 1.1.1.1  1995/10/24 10:22:09  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE UGEOM
C
C ***          Define user geometry set up
C
 
#include "geant321/gcbank.inc"
#include "geant321/gckine.inc"
#include "pvolum.inc"
 
      DIMENSION PAR( 8)
      DIMENSION ZLG(6),ALG(6),WLG(6)
      DIMENSION A(3),Z(3),WMAT(3)
      DIMENSION AF(3),ZF(3),WMATF(3)
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
C
C ***          Defines USER particular materials
C
      CALL GSMATE( 9,'ALUMINIUM$', 26.98,13.,2.7   , 8.9,37.2,0,0)
      CALL GSMATE(11,'COPPER$   ', 63.54,29.,8.96  ,1.43,14.8,0,0)
      CALL GSMATE(13,'LEAD$     ',207.19,82.,11.35 ,0.56,18.5,0,0)
      CALL GSMATE(14,'URANIUM$  ',238.03,92.,18.95 ,0.32,12. ,0,0)
      CALL GSMATE(15,'AIR$      ', 14.61,7.3,0.001205,30423.,6750.,0,0)
      CALL GSMATE(16,'VACUUM$ ',1.E-16,1.E-16,1.E-16,1.E+16,1.E+16,0,0)
      CALL GSMIXT(10,'IRON(COMPOUND)$',AF,ZF,7.8,3,WMATF)
      CALL GSMIXT(21,'BGO(COMPOUND)$',A,Z,7.1,-3,WMAT)
      CALL GSMIXT(22,'LEAD GLASS$',ALG,ZLG,5.2,6,WLG)
C
C ***          Defines USER tracking media parameters
C
C
 
      FIELDM =  0.
      IFIELD =  0
      TMAXFD =  10.
      DMAXMS =  1.
      DEEMAX =  0.05
      EPSIL  =  0.001
      STMIN  =  0.001
 
      CALL GSTMED( 1,'DEFAULT MEDIUM AIR$'    , 15 , 0 , IFIELD,
     *                FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
 
 
      CALL GSTMED( 2,'ABSORBER$'              ,IMAT, 0 , IFIELD,
     *                FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
 
 
C
C ***          Defines USER'S VOLUMES
C
      NMED1  = 1
      NMED2  = 2
      JMA = LQ(JMATE-IMAT)
      X0 = Q(JMA + 9)
      XR     =  X0/4.
 
      R1     =  20.   * XR
      R2     =  21.   * XR
 
      Z1     =  10.    * X0
      Z2     =  11.    * X0
 
 
      PAR(1) = 0.
      PAR(2) = R2
      PAR(3) = Z2
      CALL GSVOLU( 'ECAL' , 'TUBE' ,NMED1, PAR , 3 , IVOL )
 
      PAR(1) = 0.
      PAR(2) = R2
      PAR(3) = 0.5 * X0
      CALL GSVOLU( 'LEAK' , 'TUBE' ,NMED1, PAR , 3 , IVOL )
 
      PAR(1) = R1
      PAR(2) = R2
      PAR(3) = Z1
      CALL GSVOLU( 'LATR' , 'TUBE' ,NMED1, PAR , 3 , IVOL )
 
      PAR(1) = 0.
      PAR(2) = R1
      PAR(3) = Z1
      CALL GSVOLU( 'BLOC' , 'TUBE' ,NMED2, PAR , 3 , IVOL )
 
C
C ***          Position volumes within ECAL
C
 
      ZC = 0.5 * (Z1 + Z2)
      CALL GSPOS( 'LEAK' ,1, 'ECAL' , 0.   , 0.   , -ZC  , 0,'ONLY')
      CALL GSPOS( 'LEAK' ,2, 'ECAL' , 0.   , 0.   , +ZC  , 0,'ONLY')
      CALL GSPOS( 'LATR' ,1, 'ECAL' , 0.   , 0.   , 0.   , 0,'ONLY')
      CALL GSPOS( 'BLOC' ,1, 'ECAL' , 0.   , 0.   , 0.   , 0,'ONLY')
 
      CALL GSDVN( 'RTUB' , 'BLOC' ,   NR , 1)
      CALL GSDVN( 'RING' , 'RTUB' ,   NL , 3)
 
 
C ***          Close geometry banks. (obligatory system routine)
 
      CALL GGCLOS
 
      END
