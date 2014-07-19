*
* $Id: cojets.F,v 1.1.1.1 1996/01/11 14:14:34 mclareni Exp $
*
* $Log: cojets.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:34  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE COJETS(JTDKY,JTEVT,JTCOM,JTLIS)
C     ******************************************
C-- USER CALLING ROUTINE FOR COJETS
C--
C--  JTDKY = +/- UNIT NUMBER FOR DECAY TABLE FILE.
C--              IF IT IS NEGATIVE, DECAY TABLE IS NOT PRINTED.
C--  JTEVT = +/- UNIT NUMBER FOR OUTPUT EVENT FILE.
C--              IF IT IS NEGATIVE, ONLY STABLE PARTICLES ARE
C--              WRITTEN ON IT.
C--  JTCOM =     UNIT NUMBER FOR COMMAND FILE.
C--  JTLIS =     UNIT NUMBER FOR LISTING.
C--
C-- CREATED: 88/05/21
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/event.inc"
#include "cojets/intype.inc"
#include "cojets/itapes.inc"
      ITDKY=JTDKY
      ITEVT=JTEVT
      ITCOM=ABS(JTCOM)
      ITLIS=ABS(JTLIS)
C
C-- INITIALIZE (SET FLAG, INPUT OF ISAJET TYPE)
      INTYPE=1
      IF(JTEVT.GT.0) NOUNST=.FALSE.
      CALL SBLOCK
C-- CALL GENERATING ROUTINE
      CALL GEVENT
C-- PRINT PARTICLE AND DECAY TABLE
      IF(JTDKY.GT.0) CALL PARTAB
      RETURN
      END
