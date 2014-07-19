*
* $Id: wadkro.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: wadkro.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION WADKRO(IP)
C.----------------------------------------------------------------------
C.
C.    KROLL-WADA MASS DISTRIBUTION FOR VIRTUAL PHOTON IN DALITZ
C.    DECAYS                                               (MATRIX = 1)
C.    LAST UPDATE: 10/04/88
C.
C.----------------------------------------------------------------------
#include "eurodec/result.inc"
      DATA RKWNRM/1.022006E-3/
   10 WADKRO=RKWNRM*(PTEIL(5,IP)/RKWNRM)**EURRAN(INRM)
      EER=(RKWNRM/WADKRO)**2
      WTEE=SQRT(1.-EER)*(1.+.5*EER)*(1.-(WADKRO/PTEIL(5,IP))**2)**3
      IF (WTEE.LT.EURRAN(IWTEE)) GOTO 10
      RETURN
      END
