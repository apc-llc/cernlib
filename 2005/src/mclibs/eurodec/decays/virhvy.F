*
* $Id: virhvy.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: virhvy.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION VIRHVY(IP,VWMAS)
C.----------------------------------------------------------------------
C.
C.    HEAVY QUARK AND W MASS DISTRIBUTIONS IN SINGLE QUARK ONIUM
C.    DECAYS.                                             (MATRIX = 24)
C.    LAST UPDATE: 14/08/89
C.
C.----------------------------------------------------------------------
#include "eurodec/result.inc"
#include "eurodec/convrt.inc"
#include "eurodec/ptable.inc"
#include "eurodec/wekpar.inc"
      DATA DELTAM, GAMHVY/ 0.5, 0.0001/
C--
C--   DETERMINE BOUNDS ON VIRTAL QUARK MASS
      TOPUP=PTEIL(5,IP)-PTEIL(5,NTEIL+1)-DELTAM
      TOPLO=PTEIL(5,NTEIL+2)+PTEIL(5,NTEIL+3)+PTEIL(5,NTEIL+4)+DELTAM
      AMT=PTEIL(5,NTEIL+1)
      AMT2=AMT**2
      TUP=ATAN((TOPUP**2-AMT2)/AMT/GAMHVY)
      TLO=ATAN((TOPLO**2-AMT2)/AMT/GAMHVY)
C--
C--   GENERATE (VIRTUAL) HEAVY QUARK MASS
   10 VIRHVY=AMT*(AMT+GAMHVY*TAN((TUP-TLO)*EURRAN(ITDUM)+TLO))
      IF (VIRHVY.LE.0.) GOTO 10
      VIRHVY=SQRT(VIRHVY)
C--
C--   DETERMINE BOUNDS ON (VIRTAL) W MASS
      WPMUP=VIRHVY-PTEIL(5,NTEIL+2)-DELTAM
      WPMLO=PTEIL(5,NTEIL+3)+PTEIL(5,NTEIL+4)+DELTAM
      AMW2=AMW**2
      WUP=ATAN((WPMUP**2-AMW2)/AMW/GAMW)
      WLO=ATAN((WPMLO**2-AMW2)/AMW/GAMW)
C--
C--   GENERATE (VIRTUAL) W MASS
   20 VWMAS=AMW*(AMW+GAMW*TAN((WUP-WLO)*EURRAN(IWDUM)+WLO))
      IF (VWMAS.LE.0.) GOTO 20
      VWMAS=SQRT(VWMAS)
      IF (VWMAS.LE.(PTEIL(5,NTEIL+3)+PTEIL(5,NTEIL+4))) GOTO 20
      IF (VIRHVY.LE.(VWMAS+PTEIL(5,NTEIL+2))) GOTO 10
      RETURN
      END
