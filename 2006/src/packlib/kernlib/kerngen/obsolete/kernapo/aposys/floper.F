*
* $Id: floper.F,v 1.1.1.1 1996/02/15 17:51:09 mclareni Exp $
*
* $Log: floper.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:09  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
#if defined(CERNLIB_QF_APO)
      SUBROUTINE FLOPER (MODE, CHFILE, LUNOP)

C-    File operations :
C-    MODE = 1 :  delete file CHFILE
C-           2 :  change name of file CHFILE to CHFILE.bak

C-    Routine used by Patchy and CVFIXTO

      COMMON /SLATE/ LUNFLG,MSLATE(39)
      CHARACTER    CHFILE*(*),  CHLEAF*32,  WORK*256
      INTEGER*2    NOLDLG, NLEAFLG, NWORK


C--                Measure length of path-name, check file exists

      LUNFLG = 0
      NOLD   = LNBLNK (CHFILE)
      NOLDLG = NOLD
      CALL NAME_$GET_PATH (CHFILE,NOLDLG,WORK,NWORK,ISTAT)
      IF (ISTAT.NE.0)              RETURN

      IF (MODE.EQ.2)               GO TO 41
      IF (MODE.NE.1)               RETURN

C----              Delete file

      CALL NAME_$DELETE_FILE (WORK,NWORK,ISTAT)
      RETURN

C----              Rename file

C--                Find start and length of last leaf

   41 ND = ICFILA ('/',CHFILE,1,NOLD)
      IF (ND.GT.NOLD)  ND = 0
      NL = MIN (NOLD-ND, 28)
      IF (NL.EQ.0)                 RETURN

C--                Construct new leaf name, adding .BAK

      CHLEAF = CHFILE(ND+1:ND+NL) // '.bak'
      NL = NL + 4

C--                Construct new path-name and delete its file, if any

      WORK(NWORK+1:NWORK+4) = '.bak'
      NWORK = NWORK + 4
      CALL NAME_$DELETE_FILE (WORK,NWORK,ISTAT)

C--                Change name

      NOLDLG  = NOLD
      NLEAFLG = NL
      CALL NAME_$CNAME (CHFILE,NOLDLG,CHLEAF,NLEAFLG,ISTAT)
      LUNFLG = 1
      RETURN
      END
#endif
