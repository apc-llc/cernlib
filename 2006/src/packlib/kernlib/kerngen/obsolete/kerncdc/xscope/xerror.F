*
* $Id: xerror.F,v 1.1.1.1 1996/02/15 17:51:17 mclareni Exp $
*
* $Log: xerror.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:17  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ER78
 
C CERN PROGLIB# Z200    XERROR          .VERSION KERNCDC  2.06  830901
 
      COMMON/ERFLG/IF78
      LOGICAL IF78
      DATA  IF78 / .FALSE./
 
      IF78=.TRUE.
      RETURN
      END
*================================================
      SUBROUTINE ER89
 
      RETURN
      END
*==============================================
      SUBROUTINE SETERR (NERROR,ERNB)
 
      EXTERNAL ERNB
      DIMENSION IARG(6)
      DATA  IARG / 0, 0, 0, 0, 0, -1/
      ENTRY SETERC
 
 
      IARG(5)=LOCF (ERNB)
      CALL SYSTEMC (NERROR,IARG)
      RETURN
      END
