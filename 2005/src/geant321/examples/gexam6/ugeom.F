*
* $Id: ugeom.F,v 1.2 1996/02/22 13:16:32 ravndal Exp $
*
* $Log: ugeom.F,v $
* Revision 1.2  1996/02/22 13:16:32  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:19  cernlib
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
#include "geant321/gclist.inc"
#include "geant321/gcmate.inc"
#include "shobuf.inc"
C
      DIMENSION PAR( 3)
      DIMENSION ZLG(6),ALG(6),WLG(6)
      DIMENSION ABGO(3),ZBGO(3),WBGO(3)
      DIMENSION AF(3),ZF(3),WMATF(3)
C
      PARAMETER   (NV=3)
      CHARACTER*4 NAMESV(NV)
      DIMENSION   NBITSV(NV)
C
      DATA NAMESV / 'SLIX','SLIY','SLIZ' /
      DATA NBITSV /    8  ,   8  ,   8   /
C
C *** Lead glass mixture parameters
C
      DATA ZLG/  82.00,  19.00,  14.00,  11.00,  8.00,  33.00/
      DATA ALG/ 207.19,  39.102,  28.088,  22.99, 15.999,  74.922/
      DATA WLG/ .65994, .00799, .126676, .0040073,.199281, .00200485/
C
C *** BGO compound parameters
C
      DATA ABGO/208.98,72.59,15.999/
      DATA ZBGO/83.,32.,8./
      DATA WBGO/4.,3.,12./
C
C *** Iron+Nickel+Crome  compound parameters
C
      DATA AF/55.847,58.71,51.998/
      DATA ZF/26.,28.,24./
      DATA WMATF/0.703964,0.099,0.197/
C
C *** Defines USER particular materials
C
      CALL GSMATE( 1,'AIR$    ', 14.61,7.3,0.001205,30423.,6750.,0,0)
      CALL GSMATE( 2,'CARBON$   ', 12.01, 6.,2.265 ,18.8,49.9,0,0)
      CALL GSMATE( 3,'ALUMINIUM$', 26.98,13.,2.7   , 8.9,37.2,0,0)
      CALL GSMATE( 4,'COPPER$   ', 63.54,29.,8.96  ,1.43,14.8,0,0)
      CALL GSMATE( 5,'LEAD$     ',207.19,82.,11.35 ,0.56,18.5,0,0)
      CALL GSMATE( 6,'URANIUM$  ',238.03,92.,18.95 ,0.32,12. ,0,0)
      CALL GSMIXT( 7,'IRON(COMPOUND)$',AF,ZF,7.8,3,WMATF)
      CALL GSMIXT( 8,'BGO(COMPOUND)$',ABGO,ZBGO,7.1,-3,WBGO)
      CALL GSMIXT( 9,'LEAD GLASS$',ALG,ZLG,5.2,6,WLG)
C
C *** Defines USER tracking media parameters
C
      FIELDM =  0.
      IFIELD =  0
      TMAXFD = +10.0
      DMAXMS = +0.50
      DEEMAX = +0.20
      EPSIL  =  0.01
      STMIN  =  0.8
C
      CALL GSTMED( 1,'ABSORBER$',NMAT, 1 , IFIELD,FIELDM,TMAXFD,
     *                DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
C
C *** Redefine cell's size in cm
C
      JMA    = LQ(JMATE-NMAT)
      RADL   =  Q(JMA + 9)
      XBIN   =  XBIN*RADL
      YBIN   =  YBIN*RADL
      ZBIN   =  ZBIN*RADL
C
C *** Defines USER'S VOLUMES
C
      PAR(1) = 0.5*NXBIN*XBIN
      PAR(2) = 0.5*NYBIN*YBIN
      PAR(3) = 0.5*NZBIN*ZBIN
      CALL GSVOLU( 'ECAL' , 'BOX ' ,  1  , PAR , 3 , IVOL )
C
      CALL GSDVN( 'SLIX' , 'ECAL' ,   NXBIN , 1)
      CALL GSDVN( 'SLIY' , 'SLIX' ,   NYBIN , 2)
      CALL GSDVN( 'SLIZ' , 'SLIY' ,   NZBIN , 3)
C
C *** Define 'SLIZ' as a detector
C
      NWHI = 2000
      NWDI = 0
C
      CALL GSDET ('CELL','SLIZ',NV,NAMESV,NBITSV,1,NWHI,NWDI,ISET,IDET)
      CALL GSDETH('CELL','SLIZ',1,'EDEP', 32, 0.0 , 1.E+06)
C
C *** Close geometry banks. (obligatory system routine)
C
      CALL GGCLOS
C
C *** Print geometry
C
      IF(IUCOMP(4HMATE,LPRIN,NPRIN).NE.0)CALL GPRINT('MATE',0)
      IF(IUCOMP(4HTMED,LPRIN,NPRIN).NE.0)CALL GPRINT('TMED',0)
      IF(IUCOMP(4HVOLU,LPRIN,NPRIN).NE.0)CALL GPRINT('VOLU',0)
      IF(IUCOMP(4HSETS,LPRIN,NPRIN).NE.0)CALL GPRINT('SETS',0)
 
      END
