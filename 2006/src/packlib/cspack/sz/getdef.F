*
* $Id: getdef.F,v 1.1.1.1 1996/03/08 15:44:27 mclareni Exp $
*
* $Log: getdef.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:27  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_VAXVMS)
      SUBROUTINE GETDEF (CHNAME)
 
C CERN PROGLIB#         GETDEF          .VERSION KERNVAX  2.35  900731
C ORIG.  31/07/90, Federico Carminati
C Modified to avoid conflict with C RTL routines
C-    Simulate UNIX system call
 
      COMMON /SLATE/ ISTAT, ISLATE(39)
      CHARACTER    CHNAME*(*)
      INTEGER      SYS$SETDDIR
 
*     ISTAT = LIB$SYS_TRNLOG ('SYS$DISK',NDISK,CHNAME,,,%VAL(0))
      CHNAME = ' '
      CALL CZGTLG('SYS$DISK',CHNAME,'LNM$PROCESS',ISTAT)
      IF (ISTAT.NE.0)              GO TO 99
      NDISK = LENOCC(CHNAME)
 
      ISTAT = SYS$SETDDIR(%VAL(0), NDIR, CHNAME(NDISK+1:))
 
      IF (.NOT.ISTAT)              GO TO 99
 
      CHNAME(NDISK+NDIR+1:) = ' '
      ISTAT = 0
      RETURN
 
   99 CHNAME = ' '
      RETURN
      END
 
#endif
