*
* $Id: getenvf.F,v 1.1.1.1 1996/02/15 17:52:24 mclareni Exp $
*
* $Log: getenvf.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:24  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE GETENVF (NAME, TEXT)
C
C CERN PROGLIB#         GETENVF         .VERSION KERNCVX  1.08  910806
C ORIG. 06/08/91, JZ
C Fortran interface routine to getenv
C
      COMMON /SLATE/ISL(40)
      CHARACTER*(*)  NAME, TEXT

      CALL GETENV (NAME,TEXT)
      ISL(1) = LNBLNK (TEXT)
      RETURN
      END
#ifdef CERNLIB_CCGEN_GETENVF
#undef CERNLIB_CCGEN_GETENVF
#endif
#ifdef CERNLIB_CCGEN_GETENI
#undef CERNLIB_CCGEN_GETENI
#endif
