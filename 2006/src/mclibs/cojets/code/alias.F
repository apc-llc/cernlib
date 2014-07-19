*
* $Id: alias.F,v 1.1.1.1 1996/01/11 14:14:31 mclareni Exp $
*
* $Log: alias.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:31  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE ALIAS(L,S,NPTS)
C     **************************
C-- ORGANIZES ARRAYS FOR ALIAS METHOD GENERATION
C  INPUT TO SUBROUTINE: ARRAY OF S(I) I=1,...NPTS
C  OUTPUT: SAME ARRAY S(I) AND L(I)
C  ST ARRAY USED TO STACK INITIAL S(I).LT.1 (FORWARD)
C    AND S(I).GT.1 (BACKWARD)
C  ONE PLUS AND MINUS EPSILON
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DIMENSION S(250)
      INTEGER L(250),ST(250)
      ONEM=1.-1.E-5
      ONEP=1.+1.E-5
C  INITIALIZE
      IG=NPTS+1
      IS=0
      DO 1 I=1,NPTS
      S(I)=FLOAT(NPTS)*S(I)
      IF(S(I)-1.) 2,1,5
5     IG=IG-1
      ST(IG)=I
      GO TO 1
2     IS=IS+1
      ST(IS)=I
1     CONTINUE
C  MAIN LOOP
3     IF(IS.EQ.0) RETURN
      IX=ST(IS)
      IY=ST(IG)
      L(IX)=IY
      S(IY)=S(IY)-1.+S(IX)
      IF(IG.EQ.NPTS.AND.IS.EQ.1) RETURN
      IS=IS-1
C  CONSTANT REALLY ONE, CHANGED TO ACCOUNT FOR ROUNDOFF
      IF(S(IY).LT.ONEM) GO TO 7
      IF(S(IY).LT.ONEP) GO TO 8
      GO TO 3
7     IS=IS+1
      ST(IS)=ST(IG)
8     IG=IG+1
      GO TO 3
      END
