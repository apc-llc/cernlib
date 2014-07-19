*
* $Id: ugeom.F,v 1.2 1996/02/22 13:24:04 ravndal Exp $
*
* $Log: ugeom.F,v $
* Revision 1.2  1996/02/22 13:24:04  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:21  cernlib
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
C
      DIMENSION PAR( 3)
      DIMENSION ZLG(6),ALG(6),WLG(6)
      DIMENSION ABGO(3),ZBGO(3),WBGO(3)
      DIMENSION AF(3),ZF(3),WMATF(3)
C
C             Lead glass mixture parameters
C
      DATA ZLG/  82.00,  19.00,  14.00,  11.00,  8.00,  33.00/
      DATA ALG/ 207.19,  39.102,  28.088,  22.99, 15.999,  74.922/
      DATA WLG/ .65994, .00799, .126676, .0040073,.199281, .00200485 /
C
C             BGO compound parameters
C
      DATA ABGO/208.98,72.59,15.999/
      DATA ZBGO/83.,32.,8./
      DATA WBGO/4.,3.,12./
C
C             Iron+Nickel+Crome  compound parameters
C
      DATA AF/55.847,58.71,51.998/
      DATA ZF/26.,28.,24./
      DATA WMATF/0.703964,0.099,0.197/
C
C ***          Defines USER particular materials
C
      CALL GSMATE( 1,'AIR$    ', 14.61,7.3,0.001205,30423.,6750.,0,0)
      CALL GSMATE( 2,'CARBON$   ', 12.01, 6.,2.265 ,18.8,49.9,0,0)
      CALL GSMATE( 3,'ALUMINIUM$', 26.98,13.,2.7   , 8.9,37.2,0,0)
      CALL GSMATE( 4,'COPPER$   ', 63.54,29.,8.96  ,1.43,14.8,0,0)
      CALL GSMATE( 5,'LEAD$     ',207.19,82.,11.35 ,0.56,18.5,0,0)
      CALL GSMATE( 6,'URANIUM$  ',238.03,92.,18.95 ,0.32,12. ,0,0)
      CALL GSMIXT(10,'IRON(COMPOUND)$',AF,ZF,7.8,3,WMATF)
      CALL GSMIXT(21,'BGO(COMPOUND)$',ABGO,ZBGO,7.1,-3,WBGO)
      CALL GSMIXT(22,'LEAD GLASS$',ALG,ZLG,5.2,6,WLG)
C
C ***          Defines USER tracking media parameters
C
      FIELDM =  0.
      IFIELD =  0
      TMAXFD = 10.0
      DMAXMS =  0.50
      DEEMAX =  0.20
      EPSIL  =  0.01
      STMIN  =  0.80
C
      CALL GSTMED( 1,'ABSORBER$'              ,IMAT, 0 , IFIELD,
     *                FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
C
C ***          Defines USER'S VOLUMES
C
      JMA = LQ(JMATE-IMAT)
      X0 = Q(JMA + 9)
      R1     =  NRTOT*DRX0*X0
      Z1     =  NLTOT*DLX0*X0*0.5
C
      PAR(1) = 0.
      PAR(2) = R1
      PAR(3) = Z1
      CALL GSVOLU( 'ECAL' , 'TUBE' ,    1, PAR , 3 , IVOL )
C
      CALL GSDVN( 'RTUB' , 'ECAL' ,   NRTOT , 1)
      CALL GSDVN( 'RING' , 'RTUB' ,   NLTOT , 3)
C
C ***          Close geometry banks. (obligatory system routine)
C
      CALL GGCLOS
C
      END
