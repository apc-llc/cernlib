*
* $Id: uhinit.F,v 1.2 1996/02/22 13:24:07 ravndal Exp $
*
* $Log: uhinit.F,v $
* Revision 1.2  1996/02/22 13:24:07  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:20  cernlib
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
#include "geant321/gckine.inc"
#include "pvolum.inc"
#include "bootwk.inc"
C
C ***            Histograms for shower development
C
*             Open a HBOOK direct access file
*
      CALL HRFILE(34,'HBOOK','N')
*
      NBINZ=NLTOT+1
      NBINR=NRTOT+1
      ZMAX=NBINZ+1
      RMAX=NBINR+1
C
      IF(IKINE.LE.3) THEN
         EDMIN=81.
         EDMAX=101.
         ETMAX=101.
       ELSE
         EDMIN=0.
         EDMAX=110.
         ETMAX=100. + 100./PKINE(1)
      ENDIF
      CALL HBOOK1(1,'TOTAL ENERGY DEPOSITION(IN PERCENT OF E INC)$'
     *,100,EDMIN,EDMAX,0.0)
      CALL HBOOK1(6,'BACKWARD LEAKAGE(IN PERCENT OF E INC)$'
     *,100,  0., EDMAX,0.0)
      CALL HBOOK1(7,' FORWARD LEAKAGE(IN PERCENT OF E INC)$'
     *,100,  0.,EDMAX,0.0)
      CALL HBOOK1(8,' LATERAL LEAKAGE(IN PERCENT OF E INC)$'
     *,100,  0., EDMAX,0.0)
      CALL HBOOK1(9,'TOTAL ENERGY CONSERVATION(IN PERCENT OF E INC)$'
     *,100, EDMIN,ETMAX,0.0)
      CALL HBOOK1(2,'LONGIT ENERGY DEPOSITION (IN PERCENT OF E INC)$'
     *, NBINZ, 1.,ZMAX,  0.0)
      CALL HBOOK1(4,'CUMUL LONGIT ENERGY DEP. (IN PERCENT OF E INC)$'
     *, NBINZ, 1.,ZMAX,  0.0)
      CALL HBOOK1(3,'RADIAL ENERGY DEPOSITION (IN PERCENT OF E INC)$'
     *, NBINR, 1.,RMAX,  0.0)
      CALL HBOOK1(5,'CUMUL RADIAL ENERGY DEP. (IN PERCENT OF E INC)$'
     *, NBINR, 1.,RMAX,  0.0)
      CALL HBOOK1(11,'NB OF GAMMA PER PLANE$'
     *, NBINZ, 1.,ZMAX,  0.0)
      CALL HBOOK1(12,'NB OF E +   PER PLANE$'
     *, NBINZ, 1.,ZMAX,  0.0)
      CALL HBOOK1(13,'NB OF E -   PER PLANE$'
     *, NBINZ, 1.,ZMAX,  0.0)
      IF(IKINE.GT.3)
     * CALL HBOOK1(14,'NB OF OTHERS PARTICLES PER PLANE$'
     *, NBINZ, 1.,ZMAX,  0.0)
      IF(JBOOT.NE.0)
     * CALL HBOOK1(1000,'NB OF BOOT CALLS PER EVENT$',20,0.,20.,0.)
C
      CALL HBIGBI(0,4)
C
#if defined(CERNLIB_CHECK)
C
C ***          Histograms for detailed studies
C
      CALL HBOOK1(21,'TOTAL GAMMA LENGHT IN RL$'
     *,100, 0.,  10. , 0.)
      CALL HBOOK1(22,'TOTAL POSIT LENGHT IN RL$'
     *,100, 0., 5.   , 0.)
      CALL HBOOK1(23,'TOTAL ELECT LENGHT IN RL$'
     *,100, 0., 5.   , 0.)
      CALL HBOOK1(24,'TOTAL LENGHT IN RL FOR OTHER PARTICLES$'
     *,100, 0., 20.  , 0.)
C
      CALL HBOOK1(31,'NUMBER OF NEXT CALLS$'
     *,100, 0., 0.1, 0.)
      CALL HBOOK1(32,'NUMBER OF MULS CALLS$'
     *,100, 0., 0.01, 0.)
      CALL HBOOK1(33,'NUMBER OF LOSS CALLS$'
     *,100, 0., 0.01, 0.)
      CALL HBOOK1(34,'NUMBER OF FIEL CALLS$'
     *,100, 0., 0.1, 0.)
      CALL HBOOK1(35,'NUMBER OF DCAY CALLS$'
     *,100, 0., 0.1, 0.)
      CALL HBOOK1(36,'NUMBER OF PAIR CALLS$'
     *,100, 0., 0.1, 0.)
      CALL HBOOK1(37,'NUMBER OF COMP CALLS$'
     *,100, 0., 0.01, 0.)
      CALL HBOOK1(38,'NUMBER OF PHOT CALLS$'
     *,100, 0., 0.01, 0.)
      CALL HBOOK1(39,'NUMBER OF BREM CALLS$'
     *,100, 0., 0.1, 0.)
      CALL HBOOK1(  40,'NUMBER OF DRAY CALLS$'
     *,100, 0., 0.1, 0.)
      CALL HBOOK1(41,'NUMBER OF ANNI CALLS$'
     *,100, 0., 0.01, 0.)
      CALL HBOOK1(42,'NUMBER OF HADR CALLS$'
     *,100, 0., 1.00, 0.)
 
      CALL HBOOK1(51,'ENERGY DISTR OF GAMMAS(0 - 10MEV)$'
     *,100,0.,0.01,0.)
      CALL HBOOK1(52,'ENERGY DISTR OF POSITRONS(0 - 10 MEV)$'
     *,100,0.,0.01,0.)
      CALL HBOOK1(53,'ENERGY DISTR OF ELECTRONS(0 - 10 MEV)$'
     *,100,0.,0.01,0.)
      CALL HBOOK1(54,'ENERGY DISTR OF OTHERS PA(0 - 10 MEV)$'
     *,100,0.,0.01,0.)
      CALL HBOOK1(61,'ENERGY DISTR OF GAMMAS(10 -100 MEV)$'
     *,100,0.,0.1,0.)
      CALL HBOOK1(62,'ENERGY DISTR OF POSITRONS(10-100 MEV)$'
     *,100,0.,0.1,0.)
      CALL HBOOK1(63,'ENERGY DISTR OF ELECTRONS(10 - 100 MEV)$'
     *,100,0.,0.1,0.)
      CALL HBOOK1(64,'ENERGY DISTR OF OTHERS PA(10 - 100 MEV)$'
     *,100,0.,0.1,0.)
C
#endif
C
      END
