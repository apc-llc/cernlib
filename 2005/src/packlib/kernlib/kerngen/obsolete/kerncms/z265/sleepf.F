*
* $Id: sleepf.F,v 1.1.1.1 1996/02/15 17:51:53 mclareni Exp $
*
* $Log: sleepf.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:53  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE SLEEPF(ISEC)
C
C CERN PROGLIB#    Z265 SLEEPF          .VERSION KERNCMS  1.05  910308
C ORIG. 08/03/91, Miguel Marquina
C Fortran interface to SLEEP
C
        CHARACTER*2 TIME
        IF (ISEC.GT.(60*60)) THEN
          WRITE(TIME,'(I2)') ISEC/(60*60)
          CALL VMCMS('CP SLEEP '//TIME//' HR',IRET)
          ISEC = MOD(ISEC,60*60)
        ENDIF
        IF (ISEC.GT.60) THEN
          WRITE(TIME,'(I2)') ISEC/60
          CALL VMCMS('CP SLEEP '//TIME//' MIN',IRET)
          ISEC = MOD(ISEC,60)
        ENDIF
        IF (ISEC.GT.0) THEN
          WRITE(TIME,'(I2)') ISEC
          CALL VMCMS('CP SLEEP '//TIME//' SEC',IRET)
        ENDIF
      RETURN
      END
