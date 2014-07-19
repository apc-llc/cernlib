*
* $Id: amass.F,v 1.1.1.1 1996/01/11 14:14:31 mclareni Exp $
*
* $Log: amass.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:31  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION AMASS(IDEN)
C     ********************
C-- GIVES MASS OF PARTICLE WITH CODE IDEN
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/data1.inc"
      IDENA=ABS(IDEN)
      AMASS=0.
      IF(IDENA.GT.0.AND.IDENA.LE.6) THEN
        AMASS=QMAS(IDENA)
      ELSEIF(MOD(IDENA,100).EQ.0) THEN
        IQ=MOD(IDENA,1000)/100
        IF(IQ.GT.0.AND.IQ.LE.6) AMASS=QMAS(IQ)
      ELSE
        IN=INTID(IDENA)
        IF(IN.NE.0) AMASS=PMAS(IN)
      ENDIF
      RETURN
      END
