*
* $Id: listph.F,v 1.1.1.1 1996/01/11 14:14:40 mclareni Exp $
*
* $Log: listph.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:40  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE LISTPH(I)
C     ********************
C-- CALLED BY DUMPEV -- LISTS PARAMETERS OF HADRON I
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/event.inc"
#include "cojets/itapes.inc"
#include "cojets/qcds.inc"
      CHARACTER*8 LABEL,LPTCL
      DIMENSION PV(5)
C-- IF I=0, PRINT HEADING AND RETURN
      IF(I.EQ.0) THEN
      WRITE(ITLIS,41)
41    FORMAT(/7X,'I',3X,'JET',5X,'ORIG'
     C  ,5X,'TYPE',7X,'ID',10X,'DECAY',8X,'PX',8X,'PY',8X,'PZ',8X,'P0'
     C  ,8X,'PT',5X,'THETA',7X,'PHI')
        RETURN
      ENDIF
      DO 10 J=1,5
10    PV(J)=PARHAD(I,J)
      I1=ABS(IORIG(I))
      JET=I1/IPACK
      I1=I1-IPACK*JET
      I1=SIGN(I1,IORIG(I))
      IDLV1=IDENT(I)
      LPTCL=LABEL(IDLV1)
      J1=IDCAY(I)/IPACK
      J2=MOD(IDCAY(I),IPACK)
      PTHAD=SQRT(PV(1)**2+PV(2)**2)
      PHAD=SQRT(PV(3)**2+PTHAD**2)
      THHAD=0.
      IF(PHAD.GT.0.) THHAD=ACOSX(PV(3)/PHAD)
      PHIHAD=ATAN2X(PV(2),PV(1))
      IF(PHIHAD.LT.0.) PHIHAD=2.*PI+PHIHAD
      IF(IDCAY(I).EQ.0) THEN
        WRITE(ITLIS,50) I,JET,I1,LPTCL,IDLV1,(PV(K),K=1,4),PTHAD,
     $  THHAD,PHIHAD
50      FORMAT(1X,I7,I6,I9,4X,A5,4X,I5,9X,'STABLE',5F10.2,2F10.4)
      ELSE
        WRITE(ITLIS,60) I,JET,I1,LPTCL,IDLV1,J1,J2,(PV(K),K=1,4),
     $  PTHAD,THHAD,PHIHAD
60      FORMAT(1X,I7,I6,I9,4X,A5,4X,I5,4X,I5,'-',I5,5F10.2,2F10.4)
      ENDIF
      RETURN
      END
