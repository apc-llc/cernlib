*
* $Id: uhinit.F,v 1.2 1996/02/01 14:37:30 ravndal Exp $
*
* $Log: uhinit.F,v $
* Revision 1.2  1996/02/01 14:37:30  ravndal
* Clean up of the repository
*
* Revision 1.1.1.1  1995/10/24 10:22:10  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE UHINIT
C
C     *       To book the user's histograms
C     *
 
#include "pvolum.inc"
 
C
C ***            Histograms for shower development
C
      NBINZ=NL+1
      NBINR=NR+1
      ZMAX=NBINZ+1
      RMAX=NBINR+1
C
      CALL HBOOK1(1,'TOTAL ENERGY DEPOSITION$'
     *,100, 81.,101., 0.0)
      CALL HBOOK1(2,'LONGIT ENERGY DEPOSITION $'
     *, NBINZ, 1.,ZMAX,  0.0)
      CALL HBOOK1(4,'CUMUL LONGIT ENERGY DEP. $'
     *, NBINZ, 1.,ZMAX,  0.0)
      CALL HBOOK1(3,'RADIAL ENERGY DEPOSITION $'
     *, NBINR, 1.,RMAX,  0.0)
      CALL HBOOK1(5,'CUMUL RADIAL ENERGY DEP. $'
     *, NBINR, 1.,RMAX,  0.0)
      CALL HBOOK1(11,'NB OF GAMMA PER PLANE$'
     *, NBINZ, 1.,ZMAX,  0.0)
      CALL HBOOK1(12,'NB OF E +   PER PLANE$'
     *, NBINZ, 1.,ZMAX,  0.0)
      CALL HBOOK1(13,'NB OF E -   PER PLANE$'
     *, NBINZ, 1.,ZMAX,  0.0)
 
      CALL HBIGBI(0,4)
 
 
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
      CALL HBOOK1(40,'NUMBER OF DRAY CALLS$'
     *,100, 0., 0.1, 0.)
      CALL HBOOK1(41,'NUMBER OF ANNI CALLS$'
     *,100, 0., 0.01, 0.)
 
      CALL HBOOK1(51,'ENERGY DISTR OF GAMMAS$'
     *,100,0.,0.1,0.)
      CALL HBOOK1(52,'ENERGY DISTR OF POSITRONS$'
     *,100,0.,0.1,0.)
      CALL HBOOK1(53,'ENERGY DISTR OF ELECTRONS$'
     *,100,0.,0.1,0.)
      CALL HBOOK1(61,'ENERGY DISTR OF GAMMAS$'
     *,100,0.,0.01,0.)
      CALL HBOOK1(62,'ENERGY DISTR OF POSITRONS$'
     *,100,0.,0.01,0.)
      CALL HBOOK1(63,'ENERGY DISTR OF ELECTRONS$'
     *,100,0.,0.01,0.)
 
#endif
 
 
      END
