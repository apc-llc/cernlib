*
* $Id: cdsend.F,v 1.2 1996/03/12 13:08:04 cernlib Exp $
*
* $Log: cdsend.F,v $
* Revision 1.2  1996/03/12 13:08:04  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:35  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDSEND(CHFILE,NDB,IRC)
      CHARACTER*(*) CHFILE
      CHARACTER*255 SOURCE,TARGET,CHTARG
      CHARACTER*64  CHNAME
#include "hepdb/cduscm.inc"
#include "hepdb/cdnamc.inc"
*
*     Copy update file to input queues of all known servers
*
      IRC = 0
*
*     Copy update file to remote server directories
*
      LFILE  = LENOCC(CHFILE)
      SOURCE = CHFILE(1:LFILE)
      LSRCE  = LFILE
#if defined(CERNLIB_UNIX)
      LSLASH = INDEXB(CHFILE(1:LFILE),'/')
      CHNAME = CHFILE(LSLASH+1:LFILE)
#endif
#if defined(CERNLIB_VAXVMS)
      LSQBRA = INDEXB(CHFILE(1:LFILE),']')
      CHNAME = CHFILE(LSQBRA+1:LFILE)
#endif
 
      DO 10 I=1,NSERV(NDB)
#if defined(CERNLIB_UNIX)
         TARGET = CDREMQ(NDB,I)(1:LREMQ(NDB,I)) // '/' // CHNAME
         LTARGT = LREMQ(NDB,I) + LFILE - LSLASH + 1
         CALL CUTOL(TARGET(1:LTARGT))
*
*     Copy file to target directory
*
         IF(IDEBCD.GE.1) WRITE(LPRTCD,9001) SOURCE(1:LSRCE),
     +      TARGET(1:LTARGT)
         IC = SYSTEMF('cp '//SOURCE(1:LSRCE)//' '//TARGET(1:LTARGT))
*
*     Set ownership - this is for Multinet UID/GID mapping
*
         IF(INDEX(TARGET(1:LTARGT),'/tovxcrna/').NE.0)
     +   IC = SYSTEMF('chown jamie '//TARGET(1:LTARGT))
*
*     Now rename
*
         CHTARG = TARGET(1:LTARGT)
         CHTARG(LREMQ(NDB,I)+2:LREMQ(NDB,I)+3) = CDPRFX(NDB)
         CALL CUTOL(CHTARG(1:LTARGT))
         IF(IDEBCD.GE.1) WRITE(LPRTCD,9002) TARGET(1:LTARGT),
     +      CHTARG(1:LTARGT)
         IC = SYSTEMF('mv '//TARGET(1:LTARGT)//' '//CHTARG(1:LTARGT))
#endif
#if defined(CERNLIB_VAXVMS)
         TARGET = CDREMQ(NDB,I)(1:LREMQ(NDB,I)) // CHNAME
         LTARGT = LREMQ (NDB,I) + LFILE - LSQBRA
*
*     Copy file to target directory
*
         IF(IDEBCD.GE.1) WRITE(LPRTCD,9001) SOURCE(1:LSRCE),
     +      TARGET(1:LTARGT)
         CALL CDMOVF(SOURCE(1:LSRCE),TARGET(1:LTARGT),'K',IRC)
         IF(IRC.NE.0) GOTO 10
*
*     Now rename
*
         CHTARG = TARGET(1:LTARGT)
         CHTARG(LREMQ(NDB,I)+1:LREMQ(NDB,I)+2) = CDPRFX(NDB)
         IF(IDEBCD.GE.1) WRITE(LPRTCD,9002) TARGET(1:LTARGT),
     +      CHTARG(1:LTARGT)
         IC = LIB$RENAME_FILE(TARGET(1:LTARGT),CHTARG(1:LTARGT)
     +                        ,,,,,,,,,,)
         IF(.NOT.IC) THEN
            IRC = -1
         ELSE
            IRC = 0
         ENDIF
#endif
#if defined(CERNLIB_IBMVM)
         LBLANK = INDEXB(SOURCE(1:LSRCE),' ')
         TARGET = SOURCE(1:LBLANK) // CDREMQ(NDB,I)(1:LREMQ(NDB,I))
         LTARGT = LENOCC(TARGET)
         IF(IDEBCD.GE.1) WRITE(LPRTCD,9001) SOURCE(1:LSRCE),
     +      TARGET(1:LTARGT)
         CALL VMCMS('COPYFILE '//SOURCE(1:LSRCE)//' '//TARGET(1:LTARGT)
     +     ,IRC)
         CALL VMCMS('RENAME'//TARGET(1:LTARGT)//' '//
     +     CDPRFX(NDB)//TARGET(3:LTARGT),IRC)
#endif
   10 CONTINUE
 
9001  FORMAT(' CDSEND. copying  ',A,' to ',/,18X,A)
9002  FORMAT(' CDSEND. renaming ',A,' to ',/,18X,A)
      END
