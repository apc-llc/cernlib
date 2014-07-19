*
* $Id: isawnd.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: isawnd.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISAWND
C-----------------------------------------------------------------
C-
C-  Replaces ISAJET ISAWND. Write Zebra end record (1002).
C-
C-    SDP Jan.,1986
C-
C-----------------------------------------------------------------
C
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "zebcom.inc"
#include "isaunt.inc"
C----------------------------------------------------------------------
C
      CALL ISAFFL     ! fill ISAF bank
C
C  write out end record
      CALL FZOUT(ISUNIT,IXMAIN,LISAE,1,' ',1,0,0)
      CALL MZWIPE(0)
      CALL ISA_STOP
      RETURN
      END
