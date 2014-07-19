*
* $Id: stopd.F,v 1.1.1.1 1996/01/11 14:14:43 mclareni Exp $
*
* $Log: stopd.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:43  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE STOPD(IDP)
C     *********************
C-- FREEZES DECAY OF PARTICLE IDP (ISAJET ID CODE)
C-- CREATED: 88/06/15
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/inifl.inc"
#include "cojets/itapes.inc"
#include "cojets/stable.inc"
      INTP=INTID(IDP)
      IF(INTP.GT.0) THEN
        RDECAY(INTP)=0.
      ELSE
        WRITE(ITLIS,55)
55      FORMAT(/' ***BAD INPUT -- LOOK AT PARTICLE ID'
     *  ,' IN PARTICLE TABLE -- JOB WILL BE ABORTED')
        IFL=1
      ENDIF
      RETURN
      END
