*
* $Id: fermi.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: fermi.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE FERMI(PSOFT,SIGMA,TRUNC)
C.----------------------------------------------------------------------
C.
C.    GIVE SOFT PARTONS A FERMI MOTION,
C.    ASSUMING TRUNCATED GAUSSIAN DISTRIBUTION (WIDTH = SIGMA)
C.    LAST UPDATE: 14/02/89
C.
C.----------------------------------------------------------------------
#include "eurodec/picons.inc"
      DIMENSION PSOFT(3)
      PMOD=SIGMA*SQRT(-2.*LOG(MAX(EURRAN(IDUM),TRUNC)))
      PHI=TWOPI*EURRAN(IPHDUM)
      THETA=PI*EURRAN(ITHDUM)
      PSOFT(1)=PMOD*COS(THETA)
      PSOFT(2)=PMOD*SIN(PHI)*SIN(THETA)
      PSOFT(3)=PMOD*COS(PHI)*SIN(THETA)
      RETURN
      END
