*
* $Id: cdback.F,v 1.2 1996/03/12 13:07:52 cernlib Exp $
*
* $Log: cdback.F,v $
* Revision 1.2  1996/03/12 13:07:52  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:35  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDBACK(CHFILE,IDB,IRC)
      CHARACTER*(*) CHFILE
      CHARACTER*255 SOURCE,TARGET
      CHARACTER*64  CHNAME
#include "hepdb/cduscm.inc"
#include "hepdb/cdnamc.inc"
      INTEGER       SYSTEMF
 
      IRC = 0
*
*     Copy update file to backup directory
*
      LFILE  = LENOCC(CHFILE)
      SOURCE = CHFILE(1:LFILE)
      LSRCE  = LFILE
#if defined(CERNLIB_UNIX)
      LSLASH = INDEXB(CHFILE(1:LFILE),'/')
      CHNAME = CHFILE(LSLASH+1:LFILE)
      CHNAME(1:2) = CDPRFX(IDB)
      TARGET = CDSAVD(1:LSAVD) // '/' // CHNAME
      LTARGT = LSAVD + LFILE - LSLASH + 1
      CALL CUTOL(TARGET(1:LTARGT))
      IF(IDEBCD.GE.1) WRITE(LPRTCD,9001) SOURCE(1:LSRCE),
     +   TARGET(1:LTARGT)
      IRC = SYSTEMF('mv '//SOURCE(1:LSRCE)//' '//TARGET(1:LTARGT))
#endif
#if defined(CERNLIB_VAXVMS)
      LSQBRA = INDEXB(CHFILE(1:LFILE),']')
      CHNAME = CHFILE(LSQBRA+1:LFILE)
      CHNAME(1:2) = CDPRFX(IDB)
      TARGET = CDSAVD(1:LSAVD) // CHNAME
      LTARGT = LSAVD + LFILE - LSQBRA
      IF(IDEBCD.GE.1) WRITE(LPRTCD,9001) SOURCE(1:LSRCE),
     +   TARGET(1:LTARGT)
      IC = LIB$RENAME_FILE(SOURCE(1:LSRCE),TARGET(1:LTARGT),,,,,,,,,,)
      IF(.NOT.IC) THEN
         IRC = -1
      ELSE
         IRC = 0
      ENDIF
 
#endif
#if defined(CERNLIB_IBMVM)
      LBLANK = INDEXB(SOURCE(1:LSRCE),' ')
      TARGET = SOURCE(1:LBLANK) // CDSAVD(1:LSAVD)
      TARGET(1:2) = CDPRFX(IDB)
      LTARGT = LENOCC(TARGET)
      IF(LSAVD.EQ.1) THEN
         LTARGT = LTARGT + 1
         TARGET(LTARGT:LTARGT) = '1'
      ENDIF
      IF(IDEBCD.GE.1) WRITE(LPRTCD,9001) SOURCE(1:LSRCE),
     +   TARGET(1:LTARGT)
*     CALL VMCMS('COPYFILE '//SOURCE(1:LSRCE)//' '//TARGET(1:LTARGT),
*    +           IRC)
*     CALL VMCMS('ERASE '//SOURCE(1:LSRCE),IRC)
      CALL VMCMS('EXEC CDJOURN '//SOURCE(1:LSRCE)
     +                          //' '//TARGET(1:LTARGT),IRC)
#endif
9001  FORMAT(' CDBACK. renaming ',A,' to ',/,18X,A)
      END
