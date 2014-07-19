*
* $Id: eurrin.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: eurrin.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE EURRIN
C.----------------------------------------------------------------------
C.
C.    INITIALIZATION ROUTINE FOR SUPER LONG PERIOD (2**144) RANDOM
C.    NUMBER GENERATOR,
C.    TAKEN FROM:  G. MARSAGLIA AND A. ZAMAN, FSU-SCRI-87-50,
C.    MODIFICATION FOR SEEDS:  F. JAMES, CERN DD-PAPER, NOVEMBER 1988
C.    NOTE: 0 <= ISEED(1) < 31328, 0 <= ISEED(2) <30081
C.    LAST UPDATE: 05/12/88
C.
C.----------------------------------------------------------------------
#include "eurodec/rnseed.inc"
C--
C--   CHECK VALUE RANGE OF SEEDS
      IF ((ISEED(1).LT.0).OR.(ISEED(1).GE.31328)) CALL ERRORD(86,' ',FLO
     &AT(ISEED(1)))
      IF ((ISEED(2).LT.0).OR.(ISEED(2).GE.30081)) CALL ERRORD(86,' ',FLO
     &AT(ISEED(2)))
C--
C--   CALCULATE MARSAGLIA AND ZAMAN SEEDS (BY F. JAMES)
      IS1=MOD(ISEED(1)/177,177)+2
      IS2=MOD(ISEED(1),177)+2
      IS3=MOD(ISEED(2)/169,178)+1
      IS4=MOD(ISEED(2),169)
      DO 20 I=1,97
        S=0.
        T=0.5
        DO 10 J=1,24
          IS5=MOD (MOD(IS1*IS2,179)*IS3,179)
          IS1=IS2
          IS2=IS3
          IS3=IS5
          IS4=MOD(53*IS4+1,169)
          IF (MOD(IS4*IS5,64).GE.32) S=S+T
   10   T=0.5*T
   20 URAN(I)=S
      CRAN=362436./16777216.
      CDRAN=7654321./16777216.
      CMRAN=16777213./16777216.
      I97=97
      J97=33
      RETURN
      END
