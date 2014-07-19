*
* $Id: isedit.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: isedit.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION ISEDIT()
C-
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-        dummy hook for editing events written to output stream
C-
C-   Created   7-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
C-
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      LOGICAL ISEDIT
#include "zebcom.inc"
C-------------------------------------------------------
      ISEDIT=.TRUE.
      RETURN
      END

