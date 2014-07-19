*
* $Id: loren4.F,v 1.1.1.1 1996/01/11 14:14:40 mclareni Exp $
*
* $Log: loren4.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:40  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE CJLORN(PMB,V1,V2,NEW)
C     ********************************
C-- LORENTZ BOOST OF V1 TO V2 BY PMB
C-- IF NEW=0 BOOST PARAMETERS FROM  PREVIOUS CALL ARE USED
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DIMENSION PMB(4),V1(4),V2(4),BETA(3),GABETA(3)
      IF(NEW.EQ.0) GO TO 10
      DO 1 J=1,3
1     BETA(J)=PMB(J)/PMB(4)
      GA=1./SQRT(1.-BETA(1)**2-BETA(2)**2-BETA(3)**2)
      DO 4 J=1,3
4     GABETA(J)=GA*BETA(J)
      GA1GA=GA/(1.+GA)
10    CONTINUE
      BEP=BETA(1)*V1(1)+BETA(2)*V1(2)+BETA(3)*V1(3)
      DO 2 J=1,3
2     V2(J)=V1(J)+(GA1GA*BEP+V1(4))*GABETA(J)
3     V2(4)=GA*(V1(4)+BEP)
      RETURN
      END
