*
* $Id: framul.F,v 1.1.1.1 1996/01/11 14:14:37 mclareni Exp $
*
* $Log: framul.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:37  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION FRAMUL(D)
C     ******************
C-- GENERATES MULTIPLICITY ACCORDING TO KNO
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DIMENSION VKNO(250)
      INTEGER LKNO(250)
      DATA ICALL/0/
C
      IF(ICALL.GT.0) GO TO 10
      ICALL=1
      TVKNO=0.
      NB=250
      DB=10./FLOAT(NB)
      HDB=DB*.5
      DO 1 M=1,NB
      VKNO(M)=FMULDG(FLOAT(M)*DB)
1     TVKNO=TVKNO+VKNO(M)
      DO 2 M=1,NB
2     VKNO(M)=VKNO(M)/TVKNO
      CALL ALIAS(LKNO,VKNO,NB)
10    CONTINUE
C
      MX=INT(CJRN(D)*NB)+1
      IF(CJRN(D).GT.VKNO(MX)) MX=LKNO(MX)
      FRAMUL=DB*FLOAT(MX-1)+HDB
      RETURN
      END
