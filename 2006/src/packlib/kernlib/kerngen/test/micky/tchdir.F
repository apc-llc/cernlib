*
* $Id: tchdir.F,v 1.1.1.1 1996/02/15 17:54:54 mclareni Exp $
*
* $Log: tchdir.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:54  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
#if !defined(CERNLIB_QMIBM)
      SUBROUTINE TCHDIR

C-    Exercise some Unix system calls

#include "kerngent/mkch.inc"
#include "kerngent/mkcde.inc"
      COMMON /SLATE/ ISLATE(40)

      CHARACTER*128  CHCWD, CHHOME, CHNCW
      EQUIVALENCE  (CHCWD,MKCOL(1)), (CHHOME,MKCOL(129)),
     +             (CHNCW,MKCOL(257))

      INTEGER      CHDIRF


      WRITE (ITB,9020)
      MKLINE = ' '

      CALL GETWDF (CHCWD)
      NCWD = ISLATE(1)
      IF (NCWD.LE.0)               GO TO 61
      WRITE (ITB,9021) CHCWD(1:NCWD+2)

      CALL GETENVF ('HOME',CHHOME)
      NHOME = ISLATE(1)
      IF (NHOME.LE.0)              GO TO 62
      WRITE (ITB,9022) CHHOME(1:NHOME+2)

      WRITE (ITB,9023) CHHOME(1:NHOME+2)
      ISTAT =  CHDIRF (CHHOME(1:NHOME))
      IF (ISTAT.NE.0)              GO TO 63

      CALL GETWDF (CHNCW)
      NNCW = ISLATE(1)
      IF (NNCW.LE.0)               GO TO 61
      WRITE (ITB,9021) CHNCW(1:NNCW+2)

      WRITE (ITB,9023) CHCWD(1:NCWD+2)
      ISTAT =  CHDIRF (CHCWD(1:NCWD))
      IF (ISTAT.NE.0)              GO TO 63

      CALL GETWDF (CHNCW)
      NNCW = ISLATE(1)
      IF (NNCW.LE.0)               GO TO 61
      WRITE (ITB,9021) CHNCW(1:NNCW+2)
      RETURN

 9020 FORMAT (/' Try some Unix system calls'/)
 9021 FORMAT ('    get c.w.d.: ',A)
 9022 FORMAT ('      get HOME: ',A)
 9023 FORMAT ('    set c.w.d.: ',A)

C--           Failures

   61 WRITE (ITB,9061)
      GO TO 68

   62 WRITE (ITB,9062)
      GO TO 68

   63 WRITE (ITB,9063)
   68 CONTINUE
#if defined(CERNLIB_QUNIX)
      CALL PERRORF ('*** trouble')
#endif
      RETURN

 9061 FORMAT (/' !!! calling GETWDF  fails !!!')
 9062 FORMAT (/' !!! calling GETENVF fails !!!')
 9063 FORMAT (/' !!! calling CHDIRF  fails !!!')
      END
#endif
