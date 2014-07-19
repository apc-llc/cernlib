*
* $Id: charge.F,v 1.1.1.1 1996/01/11 14:14:33 mclareni Exp $
*
* $Log: charge.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:33  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION CHARGE(IDEN)
C     *********************
C-- GIVES CHARGE OF PARTICLE WITH CODE IDEN
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/data1.inc"
      DIMENSION CHRQ(6)
      DATA CHRQ/2.,-1.,-1.,2.,-1.,2./
      IDENA=ABS(IDEN)
      CHARGE=0.
      IF(IDENA.GT.0.AND.IDENA.LE.6) THEN
        CHARGE=CHRQ(IDENA)/3.
        IF(IDEN.LT.0) CHARGE=-CHARGE
      ELSEIF(MOD(IDENA,100).EQ.0) THEN
        IQ1=IDENA/1000
        IQ2=MOD(IDENA,1000)/100
        IF(IQ1.GT.0.AND.IQ1.LE.6.AND.IQ2.GT.0.AND.IQ2.LE.6)
     *  CHARGE=(CHRQ(IQ1)+CHRQ(IQ2))/3.
        IF(IDEN.LT.0) CHARGE=-CHARGE
      ELSE
        IN=INTID(IDEN)
        IF(IN.NE.0) CHARGE=LCHARG(IN)
      ENDIF
      RETURN
      END
