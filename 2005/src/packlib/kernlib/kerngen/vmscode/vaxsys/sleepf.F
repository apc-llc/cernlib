*
* $Id: sleepf.F,v 1.1.1.1 1996/02/15 17:50:26 mclareni Exp $
*
* $Log: sleepf.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:26  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE SLEEPF (NSECS)
C
C CERN PROGLIB# Z265    SLEEPF          .VERSION KERNVAX  2.36  910306
C ORIG. 06/03/91, JZ
C Fortran interface routine to sleep
C

      ASEC = NSECS
      CALL LIB$WAIT (ASEC)
      RETURN
      END
