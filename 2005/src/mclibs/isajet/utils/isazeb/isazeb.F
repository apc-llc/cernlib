*
* $Id: isazeb.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: isazeb.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISAZEB(CHIO)
C---------------------------------------------------------------
C-
C-    Initialize Zebra for Isajet
C-    Uses /ZEBCOM/ as the common block for Zebra banks
C-
C-    INPUT:
C-    CHIO = character IO control: 'I' for input, 'O' for output
C-
C-     SDP  JAN. 30,1991
C-
C---------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isaunt.inc"
#include "isabnk.inc"
#include "zebcom.inc"
#include "isalnk.inc"
      CHARACTER*1 CHIO
      LOGICAL FIRST,OK
      DATA FIRST/.TRUE./
C----------------------------------------------------------------
C
C   Do basic initalization
C
      IF(FIRST) THEN
        CALL INZCOM(2)
        FIRST=.FALSE.
C        setup the link area for ISAZEB
        CALL MZLINK(IXCOM,'/ISALNK/',LVD,PQREF(MPQREF),LVD)
C        setup the general link area
        CALL INZLNK
      ELSE
        IF(CHIO.EQ.'I') CALL FZENDI(ISUNIT,'UT')
        IF(CHIO.EQ.'O') CALL FZENDO(ISUNIT,'UT')
        CLOSE(ISUNIT)
      ENDIF
C
C    Initialize for IO
C
      IF(CHIO.EQ.'O') THEN
        CALL FZFILE (ISUNIT,0,'O')
      ELSE IF(CHIO.EQ.'I') THEN
        CALL FZFILE (ISUNIT,0,'I')
      ELSE
        PRINT 101,CHIO
        STOP
      ENDIF
      RETURN
  101 FORMAT(//'  CHIO='A4' IS NOT A RECOGNIZABLE COMMAND FOR ISAZEB,'
     $ /' JOB STOPPED')
      END
