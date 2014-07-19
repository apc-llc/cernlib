*
* $Id: cdbad.F,v 1.2 1996/03/12 13:07:54 cernlib Exp $
*
* $Log: cdbad.F,v $
* Revision 1.2  1996/03/12 13:07:54  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:35  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDBAD(CHFILE,IRC)
      CHARACTER*(*) CHFILE
      CHARACTER*255 SOURCE,TARGET
      CHARACTER*64  CHNAME
#include "hepdb/cdnamc.inc"
#include "hepdb/cduscm.inc"
      INTEGER       SYSTEMF
 
      IRC = 0
*
*     Move file to 'bad' directory
*
      LFILE  = LENOCC(CHFILE)
      SOURCE = CHFILE(1:LFILE)
      LSRCE  = LFILE
#if defined(CERNLIB_UNIX)
      LSLASH = INDEXB(CHFILE(1:LFILE),'/')
      CHNAME = CHFILE(LSLASH+1:LFILE)
      TARGET = CDBADD(1:LBADD) // '/' // CHNAME
      LTARGT = LBADD + LFILE - LSLASH + 1
      CALL CUTOL(TARGET(1:LTARGT))
      IF(IDEBCD.GE.1) WRITE(LPRTCD,9001) SOURCE(1:LSRCE),
     +   TARGET(1:LTARGT)
      IC = SYSTEMF('mv '//SOURCE(1:LSRCE)//' '//TARGET(1:LTARGT))
#endif
#if defined(CERNLIB_VAXVMS)
      LSQBRA = INDEXB(CHFILE(1:LFILE),']')
      CHNAME = CHFILE(LSQBRA+1:LFILE)
      TARGET = CDBADD(1:LBADD) // CHNAME
      LTARGT = LBADD + LFILE - LSQBRA
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
      TARGET = SOURCE(1:LBLANK) // CDBADD(1:LBADD)
      LTARGT = LENOCC(TARGET)
      IF(LBADD.EQ.1) THEN
         LTARGT = LTARGT + 1
         TARGET(LTARGT:LTARGT) = '1'
      ENDIF
      IF(IDEBCD.GE.1) WRITE(LPRTCD,9001) SOURCE(1:LSRCE),
     +   TARGET(1:LTARGT)
      CALL VMCMS('COPYFILE '//SOURCE(1:LSRCE)//' '//TARGET(1:LTARGT),
     +           IRC)
      CALL VMCMS('ERASE '//SOURCE(1:LSRCE),IRC)
#endif
9001  FORMAT(' CDBAD. renaming ',A,' to ',/,17X,A)
      END
