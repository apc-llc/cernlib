*
* $Id: list.F,v 1.1.1.1 1996/01/11 14:14:39 mclareni Exp $
*
* $Log: list.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:39  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE LIST(N)
C     ******************
C- LISTS JETGEN OUTPUT
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/data3.inc"
#include "cojets/decpar.inc"
#include "cojets/itapes.inc"
#include "cojets/jet.inc"
      CHARACTER*5 C1,C2,C3
      CHARACTER*1 CN(4),BL,MN,TWO
      EQUIVALENCE (CN,C2)
      DATA BL/' '/,MN/'-'/,TWO/'2'/
      IF(N.EQ.0) RETURN
      WRITE(ITLIS,110)
      DO 100 I=1,N
      IF(K(I,1).GT.0) C1=CHA1(K(I,1))
      IF(K(I,1).LE.0) IC1=-K(I,1)
      C3=CHA3(1)
      KA=ABS(K(I,2))
      IF(KA.LT.1000) THEN
        C2=CHA2(K(I,2))
        IF(IDB(K(I,2)).EQ.0) C3=CHA3(2)
      ELSEIF(KA.LT.10000) THEN
        KI=KA/1000
        IF(K(I,2).GT.0) C2=CHA4(KI,1)
        IF(K(I,2).LT.0) C2=CHA4(KI,2)
      ELSE
        IF(K(I,2).GT.0) CN(1)=BL
        IF(K(I,2).LT.0) CN(2)=MN
        CN(4)=TWO
        K2=KA/10000
        K3=MOD(KA/1000,10)
        CN(2)=CHA4(K2,1)
        CN(3)=CHA4(K3,1)
      ENDIF
      IF(K(I,1).GT.0) WRITE(ITLIS,120) I, C1, C2, C3, (P(I,J), J=1,5)
  100 IF(K(I,1).LE.0) WRITE(ITLIS,130) I, IC1, C2, C3, (P(I,J), J=1,5)
      RETURN
  110 FORMAT(4(' '/),T4,1HI,T17,3HORI,T24,4HPART,T32,4HSTAB,
     +T44,2HPX,T56,2HPY,T68,2HPZ,T80,1HE,T92,1HM/' ')
  120 FORMAT(1X,I4,4X,A2,1X,2(4X,A4),5(3X,E9.3))
  130 FORMAT(1X,I4,4X,1X,I2,2(4X,A4),5(3X,E9.3))
      END
