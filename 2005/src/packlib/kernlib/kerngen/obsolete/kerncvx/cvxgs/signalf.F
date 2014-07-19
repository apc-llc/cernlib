*
* $Id: signalf.F,v 1.1.1.1 1996/02/15 17:52:28 mclareni Exp $
*
* $Log: signalf.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:28  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      INTEGER FUNCTION SIGNALF (NUMSIGL,PROC,IFLAGL)
C
C CERN PROGLIB#         SIGNALF         .VERSION KERNCVX  1.08  910806
C ORIG. 06/08/91, JZ
C Fortran interface routine to signal
C
C-        NUMSIG :  signal number
C-          PROC :  external of the handler, if IFLAG = -1
C-         IFLAG :  < 0  instal PROC
C-                  = 0  default action
C-                  = 1  ignore signal
C-                  > 1  adr of handler as returned earlier
C-        function value = adr of previous handler

      INTEGER      SIGNAL
      EXTERNAL     PROC


      NUMSIG  = NUMSIGL
      IFLAG   = IFLAGL
      SIGNALF = -SIGNAL (NUMSIG,PROC,IFLAG)

      RETURN
      END
#ifdef CERNLIB_CCGEN_SIGNALF
#undef CERNLIB_CCGEN_SIGNALF
#endif
