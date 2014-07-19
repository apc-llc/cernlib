#ifndef CERNLIB_ISAJET_LUXPAR_INC
#define CERNLIB_ISAJET_LUXPAR_INC
*
*
* luxpar.inc
*
C          Parameters for RANLUX generator
C          Set by ALDATA and READIN but not by RESET
C          LUXSET=.TRUE. after RLUXGO has been called in PRTLIM
      INTEGER LUX
      PARAMETER (LUX=3)
      COMMON/LUXPAR/LUXINT,LUXK1,LUXK2,LUXGO
      INTEGER LUXINT,LUXK1,LUXK2
      LOGICAL LUXGO

#endif
