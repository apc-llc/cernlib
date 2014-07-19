*
* $Id: grot.F,v 1.1.1.1 1996/01/11 14:14:38 mclareni Exp $
*
* $Log: grot.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:38  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE GROT(N,PHIG,THETA,PF3,PL3,CG,SG)
C     *******************************************
C--  PERFORMS ROTATION FOR WEIGHT CALCULATIONS IN BRADLP
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
#include "cojets/jet.inc"
#include "cojets/qcds.inc"
C
      PHIG=PI2*CJRN(1)
      CG=COS(PHIG)
      SG=SIN(PHIG)
      COSTH=-1.+2.*CJRN(2)
      THETA=ACOSX(COSTH)
      SINTH=SIN(THETA)
      PL1=P(N,1)*CG+P(N,2)*SG
      PL3=P(N,3)*COSTH-PL1*SINTH
      PF3=P(1,3)*COSTH
      RETURN
      END
