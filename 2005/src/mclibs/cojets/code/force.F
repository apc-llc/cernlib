*
* $Id: force.F,v 1.1.1.1 1996/01/11 14:14:37 mclareni Exp $
*
* $Log: force.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:37  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE FORCE(IDP,IDS1,IDS2,IDS3,IDS4,IDS5)
C     **********************************************
C-- FORCES PARTICLE IDP TO DECAY INTO IDS1+IDS2+IDS3+IDS4+IDS5
C-- AND CHARGE CONJ. OF IDP TO DECAY INTO THE CHARGE CONJ. DECAY MODE
C-- ISAJET ID'S
C-- CREATED: 88/06/15
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/fstate.inc"
#include "cojets/inifl.inc"
#include "cojets/itapes.inc"
      IF(NFORCE.LT.100) THEN
        NFORCE=NFORCE+1
        KFORCE(1,NFORCE)=IDP
        KFORCE(2,NFORCE)=IDS1
        KFORCE(3,NFORCE)=IDS2
        KFORCE(4,NFORCE)=IDS3
        KFORCE(5,NFORCE)=IDS4
        KFORCE(6,NFORCE)=IDS5
      ELSE
        WRITE(ITLIS,58)
58      FORMAT(/' ***FORCE USED MORE THAN 100 TIMES'
     *  ,' -- JOB WILL BE ABORTED')
        IFL=1
      ENDIF
      RETURN
      END
