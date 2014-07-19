*
* $Id: gevent.F,v 1.1.1.1 1996/01/11 14:14:38 mclareni Exp $
*
* $Log: gevent.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:38  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE GEVENT
C     *****************
C-- STEERS EVENT GENERATION
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/cmpsca.inc"
#include "cojets/event.inc"
#include "cojets/evtype.inc"
#include "cojets/forgen.inc"
#include "cojets/idrun.inc"
#include "cojets/intype.inc"
#include "cojets/itapes.inc"
#include "cojets/nleave.inc"
#include "cojets/tleave.inc"
#include "cojets/tweigh.inc"
C
      IF(INTYPE.EQ.0) CALL BOOKH
      CALL TIMELF(TLEFT)
      TINIT=TLEFT
C
      IF(KMPSCA.GT.1) GO TO 100
      IF(IEVTYP.GT.1) GO TO 200
C-- PURELY JET EVENTS - SINGLE PARTON SCATTERING
10    CONTINUE
      CALL TIMELF(TLEFT)
      IF(INTYPE.EQ.0) THEN
        IF(TLEFT.LT.TLEAVE.OR.NEVENT.GE.NLEAVE) GO TO 900
      ELSE IF(INTYPE.EQ.1) THEN
        IF(TLEFT.LT.TLEAVE.OR.IEVT.GE.NLEAVE) GO TO 900
      ENDIF
      CALL HGEVEN
      GO TO 10
C
C-- W/Z, DRELL-YAN EVENTS
200   CONTINUE
      CALL TIMELF(TLEFT)
      IF(INTYPE.EQ.0) THEN
        IF(TLEFT.LT.TLEAVE.OR.NEVENT.GE.NLEAVE) GO TO 900
      ELSE IF(INTYPE.EQ.1) THEN
        IF(TLEFT.LT.TLEAVE.OR.IEVT.GE.NLEAVE) GO TO 900
      ENDIF
      CALL BGEVEN
      GO TO 200
C
C-- MULTIPARTON COLLISIONS
100   CONTINUE
      NTRIES=0
102   CALL TIMELF(TLEFT)
      IF(INTYPE.EQ.0) THEN
        IF(TLEFT.LT.TLEAVE.OR.NEVENT.GE.NLEAVE) GO TO 101
      ELSE IF(INTYPE.EQ.1) THEN
        IF(TLEFT.LT.TLEAVE.OR.IEVT.GE.NLEAVE) GO TO 101
      ENDIF
      CALL MPSCAT
      GO TO 102
101   IF(NTRIES.NE.0) SIGHRD=SIGHRD*FLOAT(NEVENT)/FLOAT(NTRIES)
      IF(NTRIES.EQ.0) SIGHRD=0.
      TENTRS=NEVENT
      GO TO 900
C
C- END RUN
900   CALL PRHEAD
      CALL OUTH
      RETURN
      END
