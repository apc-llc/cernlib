*
* $Id: cdbackup.F,v 1.4 2000/01/04 17:13:27 mclareni Exp $
*
* $Log: cdbackup.F,v $
* Revision 1.4  2000/01/04 17:13:27  mclareni
* Correct includes and remove IARGC dummy argument
*
* Revision 1.3  2000/01/04 15:56:30  jamie
* change I6 to I6.6 (blank in filename)
*
* Revision 1.2  1996/05/02 11:48:22  jamie
* increase zebra store to 3M for wa98 db
*
* Revision 1.1.1.1  1996/02/28 16:23:33  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CDBACKUP
*
*     Convert HEPDB RZ files to exchange format
*
      CHARACTER*255 CHNAMF,CHSERV,CHLINE,CHFILE,CHBACK,CHLINK
      PARAMETER (IPRINT=6)
      PARAMETER (IDEBUG=0)
      PARAMETER (LUNI=1)
      PARAMETER (LUNO=2)
      PARAMETER (LUNN=4)
      PARAMETER (NWPAW=3000000)
      COMMON/PAWC/PAW(NWPAW)
      INTEGER   SYSTEMF
 
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_TCPSOCK))
      IDUMMY = CINIT(DUMMY)
#endif
      CALL HLIMIT(NWPAW)
 
      NARGS = IARGC()
 
      IF(NARGS.LT.2) THEN
         WRITE(IPRINT,9001)
 9001 FORMAT(' CDBACKUP. Usage:',
     +       ' cdbackup source-directory target-directory')
         GOTO 999
      ENDIF
 
      CALL CDVERS(' ',' ','P')
 
      CALL XZINIT(IPRINT,IDEBUG,LUNI,LUNO)
 
      CALL GETARG(1,CHSERV)
      LSERV = LENOCC(CHSERV)
 
      CALL GETARG(2,CHBACK)
      LBACK = LENOCC(CHBACK)
 
      IF(IDEBUG.GE.1) WRITE(IPRINT,9002) CHSERV(1:LSERV),CHBACK(1:LBACK)
 9002 FORMAT(' CDBACKUP. Server directory: ',A,' Backup directory: ',A)
*
*     Open NAMES file and loop over all database files
*     (identified by :file tags)
*
#if defined(CERNLIB_UNIX)
      IF(CHSERV(LSERV:LSERV).NE.'/') THEN
         LSERV = LSERV + 1
         CHSERV(LSERV:LSERV) = '/'
      ENDIF
      IF(CHBACK(LBACK:LBACK).NE.'/') THEN
         LBACK = LBACK + 1
         CHBACK(LBACK:LBACK) = '/'
      ENDIF
#endif
 
      CHNAMF = CHSERV(1:LSERV)//'HEPDB.NAMES'
      LNAMF  = LSERV + 11
 
#if defined(CERNLIB_UNIX)
      CALL CUTOL(CHNAMF(1:LNAMF))
#endif
 
      IF(IDEBUG.GE.1) WRITE(IPRINT,9003) CHNAMF(1:LNAMF)
 9003 FORMAT(' CDBACKUP. Names file: ',A)
 
      OPEN(LUNN,FILE=CHNAMF(1:LNAMF),ACCESS='SEQUENTIAL',
     +     FORM='FORMATTED',STATUS='OLD',IOSTAT=IRC)
      IF(IRC.NE.0) GOTO 20
 
   10 CONTINUE
      READ(LUNN,'(A)',END=999) CHLINE
      LLINE = LENOCC(CHLINE)
      IF(IDEBUG.GE.2) WRITE(IPRINT,9004) CHLINE(1:LLINE)
 9004 FORMAT(' CDBACKUP. Read line: ',A,' from names file')
      IF(INDEX(CHLINE(1:LLINE),':file.').EQ.0) GOTO 10
 
      ISTART = INDEX(CHLINE(1:LLINE),':file.') + 6
      CHFILE = CHLINE(ISTART:LLINE)
      LFILE  = LLINE - ISTART + 1
      IEND   = INDEX(CHFILE(1:LFILE),' :')
      IF(IEND.NE.0) LFILE = IEND - 1
 
#if defined(CERNLIB_UNIX)
      ISTART = INDEXB(CHFILE(1:LFILE),'/') + 1
#endif
#if defined(CERNLIB_VAXVMS)
      ISTART = INDEX(CHFILE(1:LFILE),']') + 1
#endif
      IEND   = INDEXB(CHFILE(1:LFILE),'.')
 
      IF(IDEBUG.GE.1) WRITE(IPRINT,9005) CHFILE(1:LFILE)
 9005 FORMAT(' CDBACKUP. database file: ',A)
 
      CALL DATIME(ID,IT)
 
      WRITE(CHBACK(LBACK+1:),9006) CHFILE(ISTART:IEND),ID
 9006 FORMAT(A,I6.6)
      LBAKK = LBACK + IEND - ISTART + 7
 
      IRECL = 3600
      PRINT 9007,CHFILE(1:LFILE),CHBACK(1:LBAKK)
 9007 FORMAT(' CDBACKUP. copying ',A,' to ',A)
      CALL XZRTOF(CHFILE(1:LFILE),CHBACK(1:LBAKK),IRECL,' ',IRC)
 
   20 CONTINUE
      IF(IRC.NE.0) THEN
#if defined(CERNLIB_IBMVM)
         STOP 4
#endif
#if defined(CERNLIB_VAXVMS)
         CALL SYS$EXIT(%VAL(4))
#endif
#if defined(CERNLIB_UNIX)
         CALL FAEXIT(4)
#endif
      ELSE
#if defined(CERNLIB_UNIX)
*
*     Create link to latest 'good' backup
*
         CHLINK = CHBACK(1:LBAKK-6)//'good'
         LLINK  = LBAKK - 2
         CALL UNLINKF(CHLINK(1:LLINK))
         PRINT 9008,CHBACK(1:LBAKK),CHLINK(1:LLINK)
 9008 FORMAT(' CDBACKUP. linking ',A,' to ',A)
         ISTAT = SYSTEMF('ln -sf '//CHBACK(1:LBAKK)//
     +                   ' '//CHLINK(1:LLINK))
#endif
      ENDIF
 
      GOTO 10
 
  999 END
