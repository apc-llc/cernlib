*
* $Id: fmacct.F,v 1.1.1.1 1996/03/07 15:18:19 mclareni Exp $
*
* $Log: fmacct.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:19  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMACCT(CHACCT,CHACNT,IRC)
*
*     Check if CHACCT is a valid alias for CHACNT
*
*     CHACCT is typically the current account and CHACNT
*     the account in the catalogue
*
*     Accounts file has the format
*
*     emc$xv=cca$xv,rid$xv,rcl$xv
*
*     where any of the accounts in the list cca$xv,rid$xv,rcl$xv
*     are treated as emc$xv
*
      CHARACTER*(*) CHACCT,CHACNT
      CHARACTER*8   ACCOUN
      CHARACTER*255 CHFILE,CHLINE,ALIAS
      LOGICAL       IEXIST
 
#include "fatmen/fatsys.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
 
      IRC   = 0
 
      LACCT = LENOCC(CHACCT)
      LACNT = LENOCC(CHACNT)
      LDEF  = LENOCC(DEFAULT)
 
      IF(IDEBFA.GE.3) PRINT *,'FMACCT. enter for ',CHACCT(1:LACCT),' ',
     +   CHACNT(1:LACNT)
*
*     If accounts match, simply return
*
      IF(CHACCT(1:LACCT).EQ.CHACNT(1:LACNT)) RETURN
 
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
*
*     Open remote file. Here we are assuming that the remote
*     server is a Unix system
*
         CHFILE = DEFAULT(1:LDEF)//'/FATMEN.ACCOUNTS'
         LFILE  = LENOCC(CHFILE)
         CALL CUTOL(CHFILE(1:LFILE))
*
*     Does file exist?
*
         CALL XZINQR(LUFZFA,CHFILE(1:LFILE),FATNOD,ICODE,LRECL,IRC)
         IF(ICODE.NE.0) THEN
            IF(IDEBFA.GE.2) PRINT *,'FMLOCC. FATMEN.ACCOUNTS file ',
     +      'does not exist (',CHFILE(1:LFILE),')'
            GOTO 98
         ENDIF
         LRECL  = 80
         CALL XZOPEN(LUFZFA,CHFILE(1:LFILE),FATNOD,LRECL,'F',IRC)
 
      ELSE
 
#endif
#if defined(CERNLIB_UNIX)
      CHFILE = DEFAULT(1:LDEF)//'/FATMEN.ACCOUNTS'
      CALL CUTOL(CHFILE)
#endif
#if defined(CERNLIB_VAXVMS)
      CHFILE = DEFAULT(1:LDEF)//'FATMEN.ACCOUNTS'
#endif
#if defined(CERNLIB_IBMVM)
      CHFILE = '/FATMEN ACCOUNTS '//SERMOD
#endif
#if defined(CERNLIB_IBMMVS)
      CHFILE = '/'//DEFAULT(1:LDEF)//'FATMEN.ACCOUNTS'
#endif
      LFILE  = LENOCC(CHFILE)
*
*     Does file exist?
*
      INQUIRE(FILE=CHFILE(1:LFILE),EXIST=IEXIST)
 
      IF(.NOT.IEXIST) THEN
         IF(IDEBFA.GE.2) PRINT *,'FMACCT. FATMEN.ACCOUNTS file ',
     +      'does not exist (',CHFILE(1:LFILE),')'
         GOTO 98
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
      ISTAT = LIB$GET_LUN(LUNACC)
      IF(.NOT.ISTAT) THEN
         IRC = 42
         IF(IDEBFA.GE.-3) PRINT *,'FMACCT. could not assign logical ',
     +      'unit to read accounts file'
         GOTO 98
      ENDIF
 
      OPEN(LUNACC,FILE=CHFILE(1:LFILE),READONLY,ACCESS='SEQUENTIAL',
     +     STATUS='OLD',FORM='FORMATTED',IOSTAT=IRC)
 
#endif
#if defined(CERNLIB_UNIX)
      CALL CIOPEN(LUNACC,'r',CHFILE(1:LFILE),IRC)
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMACCT. error ',IRC,' opening ',
     +      CHFILE(1:LFILE)
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
      READ(LUNACC,'(A)',END=30) CHLINE
      LLINE = LENOCC(CHLINE)
#endif
#if defined(CERNLIB_UNIX)
      CALL FMCFGL(LUNACC,CHLINE,LLINE,' ',ISTAT)
      IF(ISTAT.NE.0) GOTO 30
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
 
      IF(LLINE.EQ.0) GOTO 10
      IF(IDEBFA.GE.3) PRINT *,'FMACCT. read ',CHLINE(1:LLINE)
*
*     Comments...
*
      IF(CHLINE(1:1).EQ.'!')  GOTO 10
      IF(CHLINE(1:1).EQ.'*')  GOTO 10
      IF(CHLINE(1:1).EQ.'#')  GOTO 10
*   G.Folger    "/*" is bad for cpp, so split it 
      IF(CHLINE(1:1).EQ.'/' .AND. CHLINE(2:2).EQ.'*' ) GOTO 10
 
      CALL CLTOU(CHLINE(1:LLINE))
*
*     Split line into account and alias
*
      LEQUAL = INDEX(CHLINE(1:LLINE),'=')
      IF(LEQUAL.EQ.0) GOTO 10
      ACCOUN = CHLINE(1:LEQUAL-1)
      LACC   = LEQUAL - 1
*
*     Does account match?
*
      CALL CLTOU(ACCOUN(1:LACC))
 
      IF(IDEBFA.GE.3) PRINT *,'FMACCT. account = ',ACCOUN(1:LACC)
 
      IF(ACCOUN(1:LACC).NE.CHACNT(1:LACNT)) GOTO 10
      ALIAS  = CHLINE(LEQUAL+1:LLINE)
      LALIAS = LLINE - LEQUAL
      CALL CLTOU(ALIAS(1:LALIAS))
 
      IF(IDEBFA.GE.3) PRINT *,'FMACCT. alias = ',ALIAS(1:LALIAS)
*
*     Look in list of aliases
*
      ISTART = INDEX(ALIAS(1:LALIAS),CHACCT(1:LACCT))
      IF(ISTART.EQ.0) GOTO 10
      IEND   = INDEX(ALIAS(ISTART:LALIAS),',')
      IF(IEND.EQ.0) THEN
         IEND = LALIAS
      ELSE
         IEND = IEND + ISTART - 2
      ENDIF
      IF(ALIAS(ISTART:IEND).EQ.CHACCT(1:LACCT)) GOTO 20
      GOTO 10
*
*     EOF with no match
*
30    CONTINUE
      IRC = 1
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
      CLOSE(LUNACC)
      CALL LIB$FREE_LUN(LUNACC)
#endif
#if defined(CERNLIB_UNIX)
      CALL FMCFGL(LUNACC,CHLINE,LLINE,'F',ISTAT)
      CALL CICLOS(LUNACC)
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
 
      RETURN
 
98    CONTINUE
      IRC = 1
      END
