*
* $Id: inqcds.F,v 1.1.1.1 1996/01/11 14:14:39 mclareni Exp $
*
* $Log: inqcds.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:39  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE INQCDS
C     *****************
C-- INITIALIZES QCD PARAMETERS USED BY EVOL
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/itapes.inc"
#include "cojets/qcds.inc"
C
C-- QCD CONSTANTS
      PI=4.*ATAN(1.)
      PI2=PI*2
      COLORS=3.
      CA=COLORS
      CF=(COLORS**2-1.)/(2.*COLORS)
      BALPH=(11.*COLORS-2.*FLOAT(NFLAVS))/(12.*PI)
C
C-- BASIC CONSTANTS FOR EMISSION PROBABILITIES - SPACELIKE EVOLUTION
      PRO1=PAPINT(1,ZUP)-PAPINT(1,ZLW)
      PRO2=PAPINT(2,ZUP)-PAPINT(2,ZLW)
      PRO3=PAPINT(3,ZUP)-PAPINT(3,ZLW)
      PRO3=PRO3*FLOAT(NFLAVS)
      PROGL=PRO2/(PRO2+PRO3)
      FACT=1./(2.*PI*BALPH)
      PROFLA(1)=FACT*PRO1
      PROFLA(2)=FACT*(PRO2+PRO3)
C
      RETURN
      END
