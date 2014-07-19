*
* $Id: fmacl.F,v 1.1.1.1 1996/03/07 15:18:19 mclareni Exp $
*
* $Log: fmacl.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:19  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMACL(CHUSER,CHNODE,CHPATH,CHACT,CHOPT,IRC)
*
*     Check that user@node has access to CHPATH for CHACT
*     (CHACT currently not checked)
*
      CHARACTER*(*) CHUSER,CHNODE,CHPATH,CHACT
      CHARACTER*255 CHFILE,CHLINE,PATH
      CHARACTER*20  USER,NODE
      CHARACTER*8   CHNAME
      LOGICAL       IEXIST
 
#include "fatmen/fatsys.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
#include "fatmen/fatopts.inc"
 
      IRC   = 0
 
      LUSER = LENOCC(CHUSER)
      LNODE = LENOCC(CHNODE)
      LPATH = LENOCC(CHPATH)
      LACT  = LENOCC(CHACT)
      LDEF  = LENOCC(DEFAULT)
 
      IF(IDEBFA.GE.3) PRINT *,'FMACL. enter for ',CHUSER(1:LUSER),' ',
     +   CHNODE(1:LNODE),' ',CHPATH(1:LPATH),' ',CHACT(1:LACT),' ',
     +   CHOPT
 
      CHNAME = ' '
      IF(IOPTU.NE.0) THEN
         CHNAME = 'UPDATES'
         LNAME  = 7
      ENDIF
      IF(IOPTA.NE.0) THEN
         CHNAME = 'ACL'
         LNAME  = 3
      ENDIF
 
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
*
*     Open remote file. Here we are assuming that the remote
*     server is a Unix system
*
         CHFILE = DEFAULT(1:LDEF)//'/FATMEN.'//CHNAME(1:LNAME)
         LFILE  = LENOCC(CHFILE)
         CALL CUTOL(CHFILE(1:LFILE))
*
*     Does file exist?
*
         CALL XZINQR(LUFZFA,CHFILE(1:LFILE),FATNOD,ICODE,LRECL,IRC)
         IF(ICODE.NE.0) THEN
         IF(IDEBFA.GE.2) PRINT *,'FMACL. FATMEN acl file does not ',
     +      'exist (',CHFILE(1:LFILE),')'
            GOTO 99
         ENDIF
         LRECL  = 80
         CALL XZOPEN(LUFZFA,CHFILE(1:LFILE),FATNOD,LRECL,'F',IRC)
 
      ELSE
#endif
#if defined(CERNLIB_UNIX)
      CHFILE = DEFAULT(1:LDEF)//'/FATMEN.'//CHNAME(1:LNAME)
      CALL CUTOL(CHFILE)
#endif
#if defined(CERNLIB_VAXVMS)
      CHFILE = DEFAULT(1:LDEF)//'FATMEN.'//CHNAME(1:LNAME)
#endif
#if defined(CERNLIB_IBMVM)
      CHFILE = '/FATMEN '//CHNAME(1:LNAME)//' '//SERMOD
#endif
#if defined(CERNLIB_IBMMVS)
      CHFILE = '/'//DEFAULT(1:LDEF)//'FATMEN.'//CHNAME(1:LNAME)
#endif
      LFILE  = LENOCC(CHFILE)
*
*     Does file exist?
*
      INQUIRE(FILE=CHFILE(1:LFILE),EXIST=IEXIST)
 
      IF(.NOT.IEXIST) THEN
         IF(IDEBFA.GE.2) PRINT *,'FMACL. FATMEN acl file does not ',
     +      'exist (',CHFILE(1:LFILE),')'
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
      ISTAT = LIB$GET_LUN(LUNACL)
      IF(.NOT.ISTAT) THEN
         IRC = 42
         IF(IDEBFA.GE.-3) PRINT *,'FMACL. could not assign logical ',
     +      'unit to read acl file'
         GOTO 98
      ENDIF
 
      OPEN(LUNACL,FILE=CHFILE(1:LFILE),READONLY,ACCESS='SEQUENTIAL',
     +     STATUS='OLD',FORM='FORMATTED',IOSTAT=IRC)
 
#endif
#if defined(CERNLIB_UNIX)
      CALL CIOPEN(LUNACL,'r',CHFILE(1:LFILE),IRC)
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMACL. error ',IRC,' opening ',
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
      READ(LUNACL,'(A)',END=30) CHLINE
      LLINE = LENOCC(CHLINE)
#endif
#if defined(CERNLIB_UNIX)
      CALL FMCFGL(LUNACL,CHLINE,LLINE,' ',ISTAT)
      IF(ISTAT.NE.0) GOTO 30
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
 
      IF(LLINE.EQ.0) GOTO 10
 
      IF(IDEBFA.GE.3) PRINT *,'FMACL. read ',CHLINE(1:LLINE)
*
*     Comments...
*
      IF(CHLINE(1:1).EQ.'!')  GOTO 10
      IF(CHLINE(1:1).EQ.'*')  GOTO 10
      IF(CHLINE(1:1).EQ.'#')  GOTO 10
*       G.Folger        "/*" is bad for cpp, so split it ...
      IF(CHLINE(1:1).EQ.'/' .AND. CHLINE(2:2).EQ.'*' ) GOTO 10
 
      CALL CLTOU(CHLINE(1:LLINE))
*
*     Split input line up into its component pieces
*
      IBLANK = INDEX(CHLINE(1:LLINE),' ')
      JBLANK = INDEXB(CHLINE(1:LLINE),' ')
      USER   = CHLINE(1:IBLANK-1)
      JUSER  = IBLANK - 1
      NODE   = CHLINE(IBLANK+1:JBLANK-1)
      CALL CLEFT(NODE,1,8)
      JNODE  = IS(1)
      PATH   = CHLINE(JBLANK+1:LLINE)
      JPATH  = LLINE  - JBLANK
*
*     Is there a <user> in the path?
*
      LLEFT = INDEX(PATH(1:JPATH),'<USER>')
      IF(LLEFT.NE.0) THEN
            CHLINE = PATH(1:LLEFT-1) // CHUSER(1:LUSER)
         IF(JPATH.GE.LLEFT+6) THEN
            CHLINE = PATH(1:LLEFT-1) // CHUSER(1:LUSER) //
     +               PATH(LLEFT+6:JPATH)
            PATH   = CHLINE
            JPATH  = LENOCC(PATH)
         ELSE
            PATH   = PATH(1:LLEFT-1) // CHUSER(1:LUSER)
            JPATH  = LLEFT - 1 + LUSER
         ENDIF
      ENDIF
*
*     Now match
*
      IF(USER(1:JUSER).NE.'<USER>') THEN
         CALL FMATCH(CHUSER(1:LUSER),USER(1:JUSER),IRU)
      ELSE
         IRU = 0
      ENDIF
 
      CALL FMATCH(CHNODE(1:LNODE),NODE(1:JNODE),IRN)
      CALL FMATCH(CHPATH(1:LPATH),PATH(1:JPATH)//'*',IRP)
      IF(IRU+IRN+IRP.EQ.0) GOTO 20
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
      CLOSE(LUNACL)
      CALL LIB$FREE_LUN(LUNACL)
#endif
#if defined(CERNLIB_UNIX)
      CALL FMCFGL(LUNACL,CHLINE,LLINE,'F',ISTAT)
      CALL CICLOS(LUNACL)
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
 
      RETURN
 
99    CONTINUE
      IRC = 0
      RETURN
 
98    CONTINUE
      IRC = 1
      END
