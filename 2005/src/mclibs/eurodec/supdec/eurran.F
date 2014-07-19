*
* $Id: eurran.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: eurran.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION EURRAN(IDUM)
C.----------------------------------------------------------------------
C.
C.    SUPER LONG PERIOD (2**144) RANDOM NUMBER GENERATOR.
C.    TAKEN FROM:  G. MARSAGLIA AND A. ZAMAN, FSU-SCRI-87-50,
C.    MODIFICATION FOR SEEDS:  F. JAMES, CERN DD-PAPER, NOVEMBER 1988
C.    GENERATOR IS INITIALIZED VIA A CALL TO EURRIN.
C.    LAST UPDATE: 05/12/88
C.
C.----------------------------------------------------------------------
#include "eurodec/rnseed.inc"
   10 EURRAN=URAN(I97)-URAN(J97)
      IF (EURRAN.LT.0.) EURRAN=EURRAN+1.
      URAN(I97)=EURRAN
      I97=I97-1
      IF (I97.EQ.0) I97=97
      J97=J97-1
      IF (J97.EQ.0) J97=97
      CRAN=CRAN-CDRAN
      IF (CRAN.LT.0.) CRAN=CRAN+CMRAN
      EURRAN=EURRAN-CRAN
      IF (EURRAN.LT.0.) EURRAN=EURRAN+1.
      IF (EURRAN.LE.0.) GOTO 10
      RETURN
      END
