*
* $Id: hlbino.F,v 1.1.1.1 1996/01/16 17:07:56 mclareni Exp $
*
* $Log: hlbino.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:56  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 23/10/92  11.51.54  by  R. J. Genik II
*-- Author :    R. J. Genik II   23/10/92
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION FUNCTION HLBINO(N,IR)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL FUNCTION HLBINO(N,IR)
#endif
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CALCULATE LOG OF BINOMIAL COEFFICIENTS
C-
C-   Returned value  : (DOUBLE PRECISION) LOG OF BINOMIAL COEFFICIENT
C-   Inputs  : N,R  integers
C-   Outputs : NONE
C-
C-   Created  21-OCT-1992   R. J. Genik II
C----------------------------------------------------------------------
C
      INTEGER N,IR
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION HLNFCT,DBINOM
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL BINOM
#endif
C
#include "hbook/hcdifb.inc"
C
C======================================================================
C
C     GET LOG(BC)
C
C USE STIRLING TO CHECK FOR O-FLOW, FIRST ORDER CORRECTION => 6 DIGITS
C     FOR X! AT X=100, MORE FOR X GREATER. (CF BEYER, GAMMA FUNCT)
C----------------------------------------------------------------------
C
C
      HLBINO = HLNFCT(N) - HLNFCT(IR) - HLNFCT(N-IR)
C
C----------------------------------------------------------------------
C IF WE ARE WITHIN MACHINE LIMITS, GET THE MORE ACCURATE NUMBER, DBINOM
C  AND SET IT EQUAL TO HLBINO, ON 64 BIT MACHINES,  USE BINOM.
C  (CERNLIB B100)
C----------------------------------------------------------------------
C
C
#if defined(CERNLIB_DOUBLE)
      IF (HLBINO.LT.LNBIGP) HLBINO  = LOG(DBINOM(DBLE(N),IR))
#endif
#if !defined(CERNLIB_DOUBLE)
      IF (HLBINO.LT.LNBIGP) HLBINO  = LOG(BINOM(REAL(N),IR))
#endif
C
C----------------------------------------------------------------------
      END
