*
* $Id: wrend.F,v 1.1.1.1 1996/01/11 14:15:03 mclareni Exp $
*
* $Log: wrend.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:03  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE WREND
C     ****************
C-- ADAPTED FROM ISAJET
C
C          WRITE END RECORD, TYPE 300
C          CONTAINS CROSS SECTIONS AND LUMINOSITY
C
C-- CREATED: 88/05/08
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/final.inc"
#include "cojets/itapes.inc"
#include "cojets/zevel.inc"
C
C-- START WRITING
C-- /FINAL/
      ITA=ABS(ITEVT)
      IZEVEL(1)=300
      IZEVEL(2)=1
      IZEVEL(3)=NKINF
      ZEVEL(4)=SIGF
      ZEVEL(5)=ALUM
      IZEVEL(6)=ACCEPT
      IZEVEL(7)=NRECS
      IZEVEL(8)=NEVTAP
      IL=8
      CALL BUFOUT(IL)
      WRITE(ITLIS,1010) NRECS,ITA,NEVTAP
1010  FORMAT(////' THIS RUN WROTE',I10, ' PHYSICAL RECORDS ON TAPE',I3
     1/' CORRESPONDING TO',I10,' EVENTS')
      RETURN
      END
