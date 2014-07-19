*
* $Id: rdtape.F,v 1.1.1.1 1996/01/11 14:15:04 mclareni Exp $
*
* $Log: rdtape.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:04  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE RDTAPE(IDEV,IFL)
C     ***************************
C-- ADAPTED FROM ISAJET
C
C          CALL ROUTINES TO READ AND UNPACK ISAJET DATA
C          RGENS FOR EVENTS
C          RDBEG FOR BEGINNING RECORD
C          REND FOR END RECORD
C
C-- CREATED: 88/05/08
#include "cojets/itapes.inc"
#include "cojets/rectp.inc"
#include "cojets/zevel.inc"
      ITEVT=ABS(IDEV)
      CALL RGENS(IFL)
      IF(IFL.NE.0) RETURN
      IF(IRECTP.EQ.200) CALL RDBEG
      IF(IRECTP.EQ.300) CALL REND
      RETURN
      END
