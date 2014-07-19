*
* $Id: d506dp.inc,v 1.1.1.1 1996/03/07 14:31:32 mclareni Exp $
*
* $Log: d506dp.inc,v $
* Revision 1.1.1.1  1996/03/07 14:31:32  mclareni
* Minuit
*
*
#ifndef CERNLIB_MINUIT_D506DP_INC
#define CERNLIB_MINUIT_D506DP_INC
*
*
* d506dp.inc
*
#if defined(CERNLIB_DOUBLE)
C ************ DOUBLE PRECISION VERSION *************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#if !defined(CERNLIB_DOUBLE)
C ************ SINGLE PRECISION VERSION *************
#endif

#endif
