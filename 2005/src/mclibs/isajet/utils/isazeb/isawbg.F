*
* $Id: isawbg.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: isawbg.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
       SUBROUTINE ISAWBG
C------------------------------------------------------------------
C-
C-     subroutine ISAWBG writes beginning record (1001)
C-
C-     WRITTEN BY SDP 12/85, rev. Sept.86
C-
C------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isaunt.inc"
#include "zebcom.inc"
C----------------------------------------------------------------------
C
      CALL ISABFL          ! fill ISAB bank
C
C  write out beginning record
      CALL FZOUT(ISUNIT,IXMAIN,LISAB,1,' ',1,0,0)
      CALL MZWIPE(0)
       RETURN
       END
