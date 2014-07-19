*
* $Id: fmclass.F,v 1.1.1.1 1996/03/07 15:18:20 mclareni Exp $
*
* $Log: fmclass.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:20  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMCLASS(GNAME,CHSGRP,CHNAME,CHSERV,CHOPT,IRC)
*
*     Get storage class and related info for generic name
*
*     Storage class file has following syntax:
*
*     generic-name storage-class store-name storage-server
*
      CHARACTER*(*) GNAME,CHSGRP,CHNAME,CHSERV
      CHARACTER*255 GENAM,CHFILE,CHLINE,PATH
      LOGICAL       IEXIST
 
#include "fatmen/fatsys.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
#include "fatmen/fatopts.inc"
 
      IRC    = 0
      CHSGRP = ' '
      CHNAME = ' '
      CHSERV = ' '
 
      LGN    = LENOCC(GNAME)
      GENAM  = GNAME(1:LGN)
      CALL CLTOU(GENAM(1:LGN))
 
      IF(IDEBFA.GE.3) WRITE(LPRTFA,9001) GENAM(1:LGN),CHOPT
 
      LDEF   = LENOCC(DEFAULT)
 
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
*
*     Open remote file. Here we are assuming that the remote
*     server is a Unix system
*
         CHFILE = DEFAULT(1:LDEF)//'/FATMEN.STORAGE-CLASSES'
         LFILE  = LENOCC(CHFILE)
         CALL CUTOL(CHFILE(1:LFILE))
*
*     Does file exist?
*
         CALL XZINQR(LUFZFA,CHFILE(1:LFILE),FATNOD,ICODE,LRECL,IRC)
         IF(ICODE.NE.0) THEN
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9002) CHFILE(1:LFILE)
            GOTO 99
         ENDIF
         LRECL  = 80
         CALL XZOPEN(LUFZFA,CHFILE(1:LFILE),FATNOD,LRECL,'F',IRC)
 
      ELSE
#endif
#if defined(CERNLIB_UNIX)
      CHFILE = DEFAULT(1:LDEF)//'/FATMEN.STORAGE-CLASSES'
      CALL CUTOL(CHFILE)
#endif
#if defined(CERNLIB_VAXVMS)
      CHFILE = DEFAULT(1:LDEF)//'FATMEN.STORAGE-CLASSES'
#endif
#if defined(CERNLIB_IBMVM)
      CHFILE = '/FATMEN CLASSES '//SERMOD
#endif
#if defined(CERNLIB_IBMMVS)
      CHFILE = '/'//DEFAULT(1:LDEF)//'FATMEN.STORAGE.CLASSES'
#endif
      LFILE  = LENOCC(CHFILE)
*
*     Does file exist?
*
      INQUIRE(FILE=CHFILE(1:LFILE),EXIST=IEXIST)
 
      IF(.NOT.IEXIST) THEN
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9002) CHFILE(1:LFILE)
         GOTO 99
      ENDIF
*
*     Open and read the file
*
#if defined(CERNLIB_IBMVM)
      OPEN(LUFZFA,FILE=CHFILE(1:LFILE),ACTION='READ',
     +     ACCESS='SEQUENTIAL',FORM='UNFORMATTED',
     +     STATUS='OLD',IOSTAT=IRC)
#endif
#if defined(CERNLIB_IBMMVS)
      CALL KUOPEN(LUFZFA,CHFILE(1:LFILE),'OLD',IRC)
#endif
#if defined(CERNLIB_VAXVMS)
      ISTAT = LIB$GET_LUN(LUNSCL)
      IF(.NOT.ISTAT) THEN
         IRC = 42
         IF(IDEBFA.GE.-3) WRITE(LPRTFA,9003)
         GOTO 98
      ENDIF
 
      OPEN(LUNSCL,FILE=CHFILE(1:LFILE),READONLY,ACCESS='SEQUENTIAL',
     +     STATUS='OLD',FORM='FORMATTED',IOSTAT=IRC)
 
#endif
#if defined(CERNLIB_UNIX)
      CALL CIOPEN(LUNSCL,'r',CHFILE(1:LFILE),IRC)
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) WRITE(LPRTFA,9004) IRC,CHFILE(1:LFILE)
         GOTO 98
      ENDIF
 
10    CONTINUE
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
         CALL XZGETL(LUFZFA,CHLINE,'(A)',' ',IRC)
         IF(IRC.NE.0) GOTO 30
         LLINE = LENOCC(CHLINE)
      ELSE
#endif
#if defined(CERNLIB_IBMVM)
      READ(LUFZFA,NUM=LLINE,END=30)    CHLINE
#endif
#if defined(CERNLIB_IBMMVS)
      READ(LUFZFA,'(A)',END=30) CHLINE
      LLINE = LENOCC(CHLINE)
#endif
#if defined(CERNLIB_VAXVMS)
      READ(LUNSCL,'(A)',END=30) CHLINE
      LLINE = LENOCC(CHLINE)
#endif
#if defined(CERNLIB_UNIX)
      CALL FMCFGL(LUNSCL,CHLINE,LLINE,' ',ISTAT)
      IF(ISTAT.NE.0) GOTO 30
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
 
      IF(LLINE.EQ.0) GOTO 10
 
      IF(IDEBFA.GE.3) WRITE(LPRTFA,9005) CHLINE(1:LLINE)
*
*     Comments...
*
      IF(CHLINE(1:1).EQ.'!')  GOTO 10
      IF(CHLINE(1:1).EQ.'*')  GOTO 10
      IF(CHLINE(1:1).EQ.'#')  GOTO 10
*   G.Folger    "/*" is bad for cpp, so split it
      IF(CHLINE(1:1).EQ.'/' .AND. CHLINE(2:2).EQ.'*' ) GOTO 10
 
      CALL CLTOU(CHLINE(1:LLINE))
 
      LBLANK = INDEX(CHLINE(1:LLINE),' ')
      IF(LBLANK.EQ.0) THEN
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9006) CHLINE(1:LLINE)
         GOTO 10
      ENDIF
*
*     Now match
*
      CALL FMATCH(GENAM(1:LGN),CHLINE(1:LBLANK-1),IRC)
      IF(IRC.EQ.0) THEN
         LEND   = LLINE
         JBLANK = INDEXB(CHLINE(1:LEND),' ')
         IF(JBLANK.EQ.0) THEN
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9006) CHLINE(1:LLINE)
            GOTO 10
         ENDIF
         CHSERV = CHLINE(JBLANK+1:LEND)
 
         LEND   = JBLANK - 1
 
         JBLANK = INDEXB(CHLINE(1:LEND),' ')
         IF(JBLANK.EQ.0) THEN
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9006) CHLINE(1:LLINE)
            GOTO 10
         ENDIF
         CHNAME = CHLINE(JBLANK+1:LEND)
 
         LEND   = JBLANK - 1
 
         JBLANK = INDEXB(CHLINE(1:LEND),' ')
         IF(JBLANK.EQ.0) THEN
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9006) CHLINE(1:LLINE)
            GOTO 10
         ENDIF
         CHSGRP = CHLINE(JBLANK+1:LEND)
 
         IF(LBLANK.NE.JBLANK) THEN
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9006) CHLINE(1:LLINE)
            GOTO 10
         ENDIF
         GOTO 20
      ENDIF
      GOTO 10
*
*     EOF with no match
*
30    CONTINUE
      IRC    = 1
      CHSGRP = ' '
      CHNAME = ' '
      CHSERV = ' '
20    CONTINUE
 
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
         CALL XZCLOS(LUFZFA,' ',ISTAT)
      ELSE
#endif
#if defined(CERNLIB_IBM)
      CLOSE(LUFZFA)
#endif
#if defined(CERNLIB_VAXVMS)
      CLOSE(LUNSCL)
      CALL LIB$FREE_LUN(LUNSCL)
#endif
#if defined(CERNLIB_UNIX)
      CALL FMCFGL(LUNSCL,CHLINE,LLINE,'F',ISTAT)
      CALL CICLOS(LUNSCL)
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
 
      RETURN
9001  FORMAT(' FMCLASS. enter for genam = ',A,' CHOPT = ',A)
9002  FORMAT(' FMCLASS. FATMEN storage classes file does not',
     +       ' exist (',A,')')
9003  FORMAT(' FMCLASS. could not assign logical unit to read',
     +       ' storage classes file')
9004  FORMAT(' FMCLASS. error ',I10,' opening ',A)
9005  FORMAT(' FMCLASS. read ',A)
9006  FORMAT(' FMCLASS. bad syntax in line ',A)
 
99    CONTINUE
      IRC = 0
      RETURN
 
98    CONTINUE
      IRC = 1
      END
