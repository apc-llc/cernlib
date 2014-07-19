*
* $Id: fmntol.F,v 1.1.1.1 1996/03/07 15:18:20 mclareni Exp $
*
* $Log: fmntol.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:20  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMNTOL(CHNODE,LOCCOD,CHOPT,IRC)
*
*     Return location code for node CHNODE
*
      CHARACTER*(*) CHNODE
      CHARACTER*255 CHFILE,CHLINE,PATH
      CHARACTER*20  NODE
      LOGICAL       IEXIST
 
#include "fatmen/fatsys.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
#include "fatmen/fatopts.inc"
 
      IRC    = 0
      LOCCOD = 0
 
      LNODE  = LENOCC(CHNODE)
      NODE   = CHNODE(1:LNODE)
      IF(IOPTC.EQ.0) CALL CLTOU(NODE(1:LNODE))
 
      IF(IDEBFA.GE.3) PRINT 9001,CHNODE(1:LNODE),CHOPT
 9001 FORMAT(' FMNTOL. enter for ',A,1X,A)
 
      LDEF = LENOCC(DEFAULT)
 
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
*
*     Open remote file. Here we are assuming that the remote
*     server is a Unix system
*
         CHFILE = DEFAULT(1:LDEF)//'/FATMEN.LOCATIONS'
         LFILE  = LENOCC(CHFILE)
         CALL CUTOL(CHFILE(1:LFILE))
*
*     Does file exist?
*
         CALL XZINQR(LUFZFA,CHFILE(1:LFILE),FATNOD,ICODE,LRECL,IRC)
         IF(ICODE.NE.0) THEN
            IF(IDEBFA.GE.2) PRINT 9002,CHFILE(1:LFILE)
            IRC = 1
            GOTO 40
         ENDIF
         LRECL  = 80
         CALL XZOPEN(LUFZFA,CHFILE(1:LFILE),FATNOD,LRECL,'F',IRC)
 
      ELSE
#endif
#if defined(CERNLIB_UNIX)
         CHFILE = DEFAULT(1:LDEF)//'/FATMEN.LOCATIONS'
         CALL CUTOL(CHFILE)
#endif
#if defined(CERNLIB_VAXVMS)
         CHFILE = DEFAULT(1:LDEF)//'FATMEN.LOCATIONS'
#endif
#if defined(CERNLIB_IBMVM)
         CHFILE = '/FATMEN LOCATION '//SERMOD
#endif
#if defined(CERNLIB_IBMMVS)
         CHFILE = '/'//DEFAULT(1:LDEF)//'FATMEN.LOCATIONS'
#endif
         LFILE = LENOCC(CHFILE)
*
*     Does file exist?
*
         INQUIRE(FILE=CHFILE(1:LFILE),EXIST=IEXIST)
 
         IF(.NOT.IEXIST) THEN
            IF(IDEBFA.GE.2) PRINT 9002,CHFILE(1:LFILE)
 9002 FORMAT(' FMNTOL. FATMEN locations file does not exist (',A,')')
            IRC = 1
            GOTO 40
         ENDIF
*
*     Open and read the file
*
#if defined(CERNLIB_IBMVM)
         OPEN(LUFZFA,FILE=CHFILE(1:LFILE),ACTION='READ', ACCESS=
     +   'SEQUENTIAL',FORM='UNFORMATTED', STATUS='OLD',IOSTAT=IRC)
#endif
#if defined(CERNLIB_IBMMVS)
         CALL KUOPEN(LUFZFA,CHFILE(1:LFILE),'OLD',IRC)
#endif
#if defined(CERNLIB_VAXVMS)
         ISTAT = LIB$GET_LUN(LUNLOC)
         IF(.NOT.ISTAT) THEN
            IRC = 42
            IF(IDEBFA.GE.-3) PRINT *,'FMNTOL. could not assign logical '
     +      , 'unit to read locations file'
            GOTO 40
         ENDIF
 
         OPEN(LUNLOC,FILE=CHFILE(1:LFILE),READONLY,ACCESS='SEQUENTIAL',
     +   STATUS='OLD',FORM='FORMATTED',IOSTAT=IRC)
 
#endif
#if defined(CERNLIB_UNIX)
         CALL CIOPEN(LUNLOC,'r',CHFILE(1:LFILE),IRC)
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT 9003,IRC,CHFILE(1:LFILE)
 9003 FORMAT(' FMNTOL. error ',I10,' opening ',A)
         IRC = 2
         GOTO 40
      ENDIF
 
   10 CONTINUE
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
         CALL XZGETL(LUFZFA,CHLINE,'(A)',' ',IRC)
         IF(IRC.NE.0) GOTO 20
         LLINE = LENOCC(CHLINE)
      ELSE
#endif
#if defined(CERNLIB_IBMVM)
         READ(LUFZFA,NUM=LLINE,END=20) CHLINE
#endif
#if defined(CERNLIB_IBMMVS)
         READ(LUFZFA,'(A)',END=20) CHLINE
         LLINE = LENOCC(CHLINE)
#endif
#if defined(CERNLIB_VAXVMS)
         READ(LUNLOC,'(A)',END=20) CHLINE
         LLINE = LENOCC(CHLINE)
#endif
#if defined(CERNLIB_UNIX)
         CALL FMCFGL(LUNLOC,CHLINE,LLINE,' ',ISTAT)
         IF(ISTAT.NE.0) GOTO 20
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
 
      IF(LLINE.EQ.0) GOTO 10
 
      IF(IDEBFA.GE.3) PRINT *,'FMNTOL. read ',CHLINE(1:LLINE)
*
*     Comments...
*
      IF(CHLINE(1:1).EQ.'!')  GOTO 10
      IF(CHLINE(1:1).EQ.'*')  GOTO 10
      IF(CHLINE(1:1).EQ.'#')  GOTO 10
*        G.Folger       "/*" is bad for cpp, so split it...
      IF(CHLINE(1:1).EQ.'/' .AND. CHLINE(2:2).EQ.'*' ) GOTO 10
 
      IF(IOPTC.EQ.0) CALL CLTOU(CHLINE(1:LLINE))
 
      IEQUAL = INDEX(CHLINE(1:LLINE),'=')
      IF(IEQUAL.EQ.0) GOTO 10
 
      IF(CHLINE(1:IEQUAL-1).EQ.NODE(1:LNODE)) THEN
         LOCCOD = ICDECI(CHLINE,IEQUAL+1,LLINE)
         GOTO 30
      ENDIF
 
      GOTO 10
*
*     EOF with no match
*
   20 CONTINUE
      IRC = 3
   30 CONTINUE
 
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
         CALL XZCLOS(LUFZFA,' ',ISTAT)
      ELSE
#endif
#if defined(CERNLIB_IBM)
         CLOSE(LUFZFA)
#endif
#if defined(CERNLIB_VAXVMS)
         CLOSE(LUNLOC)
         CALL LIB$FREE_LUN(LUNLOC)
#endif
#if defined(CERNLIB_UNIX)
         CALL FMCFGL(LUNLOC,CHLINE,LLINE,'F',ISTAT)
         CALL CICLOS(LUNLOC)
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
 
      RETURN
 
   40 CONTINUE
 
      END
