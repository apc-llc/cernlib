*
* $Id: uhinit.F,v 1.2 1996/02/22 13:32:46 ravndal Exp $
*
* $Log: uhinit.F,v $
* Revision 1.2  1996/02/22 13:32:46  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:25  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UHINIT
*
************************************************************************
*                                                                      *
*             To book the user's histograms                            *
*                                                                      *
************************************************************************
#include "hboot.inc"
#include "tstcom.inc"
*
      IF (IENERG .EQ. 0) THEN
         CALL HBOOK1(201,'# OF HITS PER EVENT$',100,0.0,100.,0.)
         CALL HBOOK1(202,'ENERGY PER HIT$',100,0.0,0.0002,0.)
         CALL HBOOK1(203,'TOTAL ENERGY DEPOSIT IN GAS$',100,0.0,0.002,
     +   0.)
         CALL HBOOK1(204,'LONG. ENERGY DISTR.$',60,0.,60.,0.)
         CALL HBOOK1(205,'TRANS. ENERGY DISTR.$',58,-29.,29.,0.)
      END IF
*
      IF (LHBOOT(1) .EQ. 1) THEN
         CALL HBOOK1(31,'ENERGY DISTR. OF BS E+E-$',100,0.0,1.,0.)
         CALL HBOOK1(32,'COS THETA DISTR. OF BS E+E-$',100,-1.,1.,0.)
         IF (LHBOOT(2) .EQ. 1) THEN
            CALL HBOOK1(41,'ENERGY DISTR. OF BS PION$',100,0.1,0.6,0.)
            CALL HBOOK1(42,'COS THETA DISTR. OF BS PION$',100,-1.,1.,
     +      0.)
         END IF
 
      ELSE
 
         CALL HBOOK1(24,'# OF HITS PER EVENT$',100,0.,100.,0.)
         CALL HBOOK1(25,'Z VERTEX DISTRIBUTION$',100,0.,1.,0.)
         CALL HBOOK1(50,'VERTEX DISTRI. OF EVENT WITH SIGNAL$',100,
     +   11.07, 12.07,0.)
 
      END IF
*
      END
