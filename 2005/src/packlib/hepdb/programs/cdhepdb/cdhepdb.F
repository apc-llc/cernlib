*
* $Id: cdhepdb.F,v 1.1.1.1 1996/02/28 16:23:33 mclareni Exp $
*
* $Log: cdhepdb.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:33  mclareni
* Hepdb, cdlib, etc
*
*
#include "sys/CERNLIB_machine.h"
#include "_hepdb/pilot.h"
      PROGRAM CDHEPDB
*CMZ :          21/02/91  16.24.17  by  Jamie Shiers
*-- Author :    Jamie Shiers   21/02/91
*     Program to move updates between CERNVM and HEPDB
*
*     Stolen from FATMEN.
*
      PARAMETER     (NDIR=100)
      CHARACTER*255 CHDIRS(NDIR)
      PARAMETER     (NMAX=500)
      CHARACTER*64  FILES(NMAX)
      CHARACTER*8   HEPUSR,HEPNOD,REMUSR,REMNOD,REMDBS
      CHARACTER*64  REMOTE,TARGET
      CHARACTER*12  CHTIME
      CHARACTER*8   CHUSER,CHPASS
      CHARACTER*8   CHNODE,CHTYPE,CHSYS,CHRAND
      CHARACTER*6   CHENT
      CHARACTER*80  CHMAIL,LINE,CHDIR
      CHARACTER*38  VALID
      CHARACTER*255 ERRMSG
      CHARACTER*2   CDPREF
      CHARACTER*255 CDFILE
      COMMON/PAWC/PAW(50000)
      PARAMETER     (IPRINT=6)
      PARAMETER     (IDEBUG=0)
      PARAMETER     (LUNI=1)
      PARAMETER     (LUNO=2)
#include "quest.inc"
#include "slate.inc"
      DATA          NENTRY/0/
      DATA          VALID/'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890._'/
*
*     Initialise ZEBRA
*
      CALL HLIMIT(50000)
*
*     Initialise XZ
*
      CALL XZINIT(IPRINT,IDEBUG,LUNI,LUNO)
*
      CALL CDHOST(CHNODE,IRC)
      LNODE = LENOCC(CHNODE)
*
*     Open connection to HEPDB...
*
#if defined(CERNLIB_TCPSOCK)
      IDUMMY = CINIT(IDUMMY)
#endif
#if !defined(CERNLIB_TCPSOCK)
      CALL VMREXX('F','USER',CHUSER,IC)
      CALL VMREXX('F','PWD' ,CHPASS,IC)
      CALL CUTOL(CHUSER)
      CALL CUTOL(CHPASS)
      CALL VMSTAK(CHPASS,'L',IC)
      CALL VMSTAK(CHUSER,'L',IC)
#endif
 
      CALL CZOPEN('zserv','HEPDB',IRC)
*
*     First entry: look on hepdb before sleeping
*
      NDIRS = 0
      GOTO 20
 
   10 CALL VMCMS('EXEC HDBSERV',IRC)
      IF(IRC.EQ.99) GOTO 20
      IF(IRC.NE.0) THEN
         PRINT *,'CDHEPDB. error ',IRC,' from HDBSERV. Stopping...'
         GOTO 90
      ENDIF
 
      NENTRY = NENTRY + 1
*
*     Get the user and node name for this file...
*
      CALL VMCMS('GLOBALV SELECT *EXEC STACK HDBADDR',IC)
      CALL VMRTRM(LINE,IEND)
      ISTART = ICFNBL(LINE,1,IEND)
      CALL CDWORD(HEPUSR,0,' ',LINE(ISTART:IEND),IC)
      LHEP   = LENOCC(HEPUSR)
      CALL CDWORD(HEPNOD,1,' ',LINE(ISTART:IEND),IC)
      LNOD   = LENOCC(HEPNOD)
*
*     Get file name (for database prefix and name of remote server)
*
      CALL VMCMS('GLOBALV SELECT *EXEC STACK CDFILE',IC)
      CALL VMRTRM(CDFILE,LFILE)
      CDPREF = CDFILE(1:2)
      LBLANK = INDEX(CDFILE(1:LFILE),' ')
      JBLANK = INDEXB(CDFILE(1:LFILE),' ')
      REMDBS = CDFILE(LBLANK+1:JBLANK-1)
      LDBS   = JBLANK - LBLANK - 1
 
      IF(IDEBUG.GE.1)
     +PRINT *,'CDHEPDB. Update received for ',REMDBS(1:LDBS),' prefix ',
     +         CDPREF
*
*     Number of pending files
*
      CALL VMCMS('GLOBALV SELECT *EXEC STACK HDBFILES',IC)
      CALL VMRTRM(LINE,IEND)
      NRDR = ICDECI(LINE,1,IEND)
 
      CALL DATIME(ID,IT)
      WRITE(CHTIME,'(I6.6,I4.4,I2.2)') ID,IT,IS(6)
      WRITE(CHENT ,'(I6.6)') NENTRY
      CALL CDRAND(CHRAND,IRC)
*
*    Now put this file...
*    This assumes the HEPDB naming convention: /hepdb/cdgroup,
*                                          e.g. /hepdb/cdchorus
      CHDIR  = '/hepdb/'//REMDBS(1:LDBS)//
     +         '/todo'
      LDIR   = LENOCC(CHDIR)
*
      REMOTE = ' '
      REMOTE = 'zz'//CHTIME//CHRAND//CHENT
     +         //'.'//HEPUSR(1:LHEP)//'_'//HEPNOD(1:LNOD)
      LREM   = LENOCC(REMOTE)
      TARGET = REMOTE(1:LREM)
*
*     Change remote directory
*
      CALL CUTOL(CHDIR(1:LDIR))
      IF(IDEBUG.GE.1) PRINT *,'CDHEPDB. Changing remote directory to ',
     +   CHDIR(1:LDIR)
      CALL XZCD(CHDIR(1:LDIR),IRC)
      IF(IRC.NE.0) THEN
         WRITE(ERRMSG,9001) CHDIR(1:LDIR)
 9001 FORMAT(' CDHEPDB. error ',I10,' changing remote directory to ',A)
         LMSG = LENOCC(ERRMSG)
         GOTO 100
      ENDIF
 
      IF(IDEBUG.GE.1) PRINT *,'CDHEPDB. Sending file as ',
     +   REMOTE(1:LREM)
      CALL XZPUTA(CDFILE(1:LFILE),REMOTE(1:LREM),' ',IC)
      IF(IC.NE.0) THEN
         WRITE(ERRMSG,9002) IC,HEPUSR,HEPNOD
 9002    FORMAT(' CDHEPDB. error ',I6,' sending update from ',
     +            A,' at ',A,' to HEPDB')
         LMSG = LENOCC(ERRMSG)
         GOTO 100
      ENDIF
*
*     Rename the remote update file
*
      LSTA = INDEXB(TARGET(1:LREM),'/') + 1
      TARGET(LSTA:LSTA+1) = CDPREF
      IF(IDEBUG.GE.1) PRINT *,'CDHEPDB. Renaming file to ',
     +   TARGET(1:LREM)
      CALL XZMV(REMOTE(1:LREM),TARGET(1:LREM),' ',IRC)
      IF(IRC.NE.0) THEN
         WRITE(ERRMSG,9003) IRC,REMOTE(1:LREM)
 9003 FORMAT(' CDHEPDB. error ',I10,' renaming ',A)
         LMSG = LENOCC(ERRMSG)
         GOTO 100
      ENDIF
*
*     Delete this update...
*
      CALL VMCMS('ERASE '//CDFILE(1:LFILE),IC)
*
*     Try to clear out RDR
*
      IF(NRDR.GT.10) GOTO 10
*
*     Are there any files for us to get?
*
   20 CONTINUE
*
*     Get list of remote directories
*
      JCONT  = 0
      IF(NDIRS.EQ.0) THEN
         IF(IDEBUG.GE.1) PRINT 9004
 9004 FORMAT(' CDHEPDB. Retrieving list of remote directories...')
         CALL XZLS('/hepdb/cd*/tovm',CHDIRS,NDIR,NDIRS,JCONT,'D',IC)
         NDIRS = MIN(NDIR,NDIRS)
         IF(JCONT.NE.0) THEN
            IC = 0
            PRINT 9005
 9005 FORMAT(' CDHEPDB. too many directories - excess names',
     +       ' will be flushed')
*
   30       CONTINUE
            CALL CZGETA(CHMAIL,ISTAT)
            LCH = LENOCC(CHMAIL)
            IF(CHMAIL(1:1).EQ.'0') THEN
*
*        Nop
*
            ELSEIF(CHMAIL(1:1).EQ.'1') THEN
            ELSEIF(CHMAIL(1:1).EQ.'2') THEN
               GOTO 30
            ELSEIF(CHMAIL(1:1).EQ.'3') THEN
               IQUEST(1) = 1
               IRC = 1
            ELSEIF(CHMAIL(1:1).EQ.'E') THEN
               IQUEST(1) = -1
               IRC = -1
            ELSEIF(CHMAIL(1:1).EQ.'V') THEN
               GOTO 30
            ELSE
               IQUEST(1) = 1
               IRC = 1
            ENDIF
 
         ENDIF
 
         IF(IC.NE.0) THEN
            WRITE(ERRMSG,9006) IC
 9006 FORMAT(' CDHEPDB. error ',I10,' issuing remote LS command')
            LMSG = LENOCC(ERRMSG)
            GOTO 100
         ENDIF
*
      ENDIF
 
      IF(NDIRS.EQ.1.AND.INDEX(CHDIRS(1),'not found').NE.0) THEN
         ERRMSG = 'CDHEPDB. there are no remote directories!'
         LMSG   = LENOCC(ERRMSG)
         GOTO 100
      ENDIF
 
      DO 80 J=1,NDIRS
 
         LDIR = LENOCC(CHDIRS(J))
         IF(LDIR.EQ.0) GOTO 80
         CALL CLTOU(CHDIRS(J)(1:LDIR))
*
*     Get the name of the server for whom these updates are intended...
*
         JSTART = INDEX(CHDIRS(J)(1:LDIR),'/CD')
         IF(JSTART.EQ.0) THEN
            IF(IDEBUG.GE.-3)
     +      PRINT *,'CDHEPDB. unrecognised directory - skipped ',
     +      '(',CHDIRS(J)(1:LDIR),')'
            GOTO 80
         ELSE
            JSTART = JSTART + 1
         ENDIF
 
         JEND = INDEX(CHDIRS(J)(JSTART:LDIR),'/')
 
         IF(JEND.EQ.0) THEN
            PRINT *,'CDHEPDB. unrecognised file name - skipped ',
     +      '(',CHDIRS(J)(1:LDIR),')'
            GOTO 80
         ENDIF
 
         REMUSR = CHDIRS(J)(JSTART:JSTART+JEND-2)
         LREM = LENOCC(REMUSR)
 
         IF(LREM.EQ.0) THEN
            IF(IDEBUG.GE.-3)
     +      PRINT *,'CDHEPDB. unrecognised file name - skipped ',
     +      '(',CHDIRS(J)(1:LDIR),')'
            GOTO 80
         ENDIF
 
         IF(IDEBUG.GE.1)
     +   PRINT *,'CDHEPDB. processing updates for ',REMUSR(1:LREM)
         CALL XZCD(CHDIRS(J)(1:LDIR),IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBUG.GE.-3)
     +      PRINT 9007,CHDIRS(J)(1:LDIR)
 9007 FORMAT(' CDHEPDB. cannot set directory to ',A)
            LMSG = LENOCC(ERRMSG)
            GOTO 100
         ENDIF
 
         ICONT  = 0
         NFILES = 0
         IF(IDEBUG.GE.1) PRINT *, 'HEPDB. Retrieving list '
     +   //'of remote files in ', CHDIRS(J)(1:LDIR)
         CALL XZLS(' ',FILES,NMAX,NFILES,ICONT,' ',IC)
         NFILES = MIN(NFILES,NMAX)
         IF(IDEBUG.GE.2)
     +   PRINT *,'CDHEPDB. ',NFILES,' files found in ',CHDIRS(J)(1:LDIR)
         IF(ICONT.NE.0) THEN
            IC = 0
            IF(IDEBUG.GE.0)
     +      PRINT 9008
 9008 FORMAT(' CDHEPDB. too many files - excess names will be',
     +       ' flushed')
*
   40       CONTINUE
            CALL CZGETA(CHMAIL,ISTAT)
            LCH = LENOCC(CHMAIL)
            IF(CHMAIL(1:1).EQ.'0') THEN
*
*        Nop
*
            ELSEIF(CHMAIL(1:1).EQ.'1') THEN
            ELSEIF(CHMAIL(1:1).EQ.'2') THEN
               GOTO 40
            ELSEIF(CHMAIL(1:1).EQ.'3') THEN
               IQUEST(1) = 1
               IRC = 1
            ELSEIF(CHMAIL(1:1).EQ.'E') THEN
               IQUEST(1) = -1
               IRC = -1
            ELSEIF(CHMAIL(1:1).EQ.'V') THEN
               GOTO 40
            ELSE
               IQUEST(1) = 1
               IRC = 1
            ENDIF
*
         ENDIF
 
 
         DO 70 I=1,NFILES
            LF = LENOCC(FILES(I))
            IF(LF.EQ.0) GOTO 70
            CALL CLTOU(FILES(I))
*
*     Fix for the case when there are no files...
*
            IF(NFILES.EQ.1) THEN
 
               IF(INDEX(FILES(I)(1:LF),'DOES NOT EXIST').NE.0.OR.
     +         INDEX(FILES (I)(1:LF),'NOT FOUND').NE.0) GOTO 10
 
               IF(INDEX(FILES(I)(1:LF),'ARG LIST TOO LONG').NE.0) THEN
                  IF(IDEBUG.GE.-3) THEN
                     PRINT *,'CDHEPDB. Stopping due to the following '
     +               //'error...'
                     PRINT *,FILES(I)(1:LF)
                     PRINT *,'(Intervention required on HEPDB)'
                  ENDIF
                  CALL VMCMS('EXEC TELL JAMIE '//FILES(I)(1:LF),IC)
                  CALL VMCMS('EXEC TELL JAMIE Logging off...',IC)
                  CALL VMCMS('EXEC TELL FATONE Logging off due to'//
     +            FILES(I)(1:LF),IC)
                  CALL VMSTAK('LOGOFF','L',IC)
                  STOP
               ENDIF
 
            ENDIF
*
*     Check that file name is valid
*
            DO 50 L=1,LF
               IF(INDEX(VALID,FILES(I)(L:L)).EQ.0) THEN
                  IF(IDEBUG.GE.-3) THEN
                     PRINT *,'CDHEPDB. invalid character ',
     +                  FILES(I)(L:L),
     +                  ' at ',L,' in ',FILES(I)(1:LF)
                     PRINT *,'CDHEPDB. skipping update...'
                  ENDIF
                  GOTO 70
               ENDIF
   50       CONTINUE
 
            IF(INDEX(FILES(I)(1:LF),CHNODE(1:LNODE)).NE.0) THEN
               IF(IDEBUG.GE.1)
     +         PRINT *,'CDHEPDB. skipping update for ',CHNODE(1:LNODE),
     +         '(',FILES(I)(1:LF),')'
               GOTO 70
            ENDIF
            LSLASH = INDEXB(FILES(I)(1:LF),'/')
            IF(FILES(I)(LSLASH+1:LSLASH+2).EQ.'ZZ') THEN
               IF(IDEBUG.GE.1)
     +         PRINT *,'CDHEPDB. active file - skipped ', '(',FILES(I)
     +         (1:LF),')'
               GOTO 70
            ENDIF
 
            IF(IDEBUG.GE.2)
     +      PRINT *,'CDHEPDB. update found for ',REMUSR(1:LREM), '(',
     +      FILES(I)(1:LF),')'
 
            IF(IDEBUG.GE.1) PRINT *,'CDHEPDB. retrieving update ',
     +      FILES(I)(1:LF)
 
            CDPREF = FILES(I)(1:LF)
            CALL CDRAND(CHRAND,IRC)
            CDFILE = CDPREF // CHRAND(3:) // '.HEPDB.B'
            LFILE  = 16
 
            CALL XZGETA(CDFILE(1:LFILE),FILES(I)(1:LF),'S',IC)
            IF(IC.NE.0) THEN
               WRITE(ERRMSG,9009) IC,REMUSR(1:LREM)
 9009    FORMAT(' CDHEPDB. error ',I6,' retrieving update for ',A)
               LMSG = LENOCC(ERRMSG)
               GOTO 100
            ENDIF
*
*      Protection against zero length files
*
            IF(IQUEST(11).EQ.0) GOTO 60
 
            LDOT = INDEX(CDFILE(1:LFILE),'.')
            CDFILE(LDOT:LDOT) = ' '
            LDOT = INDEX(CDFILE(1:LFILE),'.')
            CDFILE(LDOT:LDOT) = ' '
 
            CALL VMCMS('EXEC SENDFILE '//CDFILE(1:LFILE)//' TO '
     +                 //REMUSR(1:LREM),IC)
            IF(IC.NE.0) THEN
               WRITE(ERRMSG,9010) IC,REMUSR(1:LREM)
 9010    FORMAT(' CDHEPDB. error ',I6,' sending update to ',A)
               LMSG = LENOCC(ERRMSG)
               GOTO 100
            ENDIF
*
*     Now delete local file
*
            CALL VMCMS('ERASE '//CDFILE(1:LFILE),IC)
*
*     and the remote one
*
   60       continue
            CALL XZRM(FILES(I)(1:LF),IC)
            IF(IC.NE.0) THEN
               WRITE(ERRMSG,9011) IC,FILES(I)(1:LF)
 9011    FORMAT(' CDHEPDB. error ',I6,' deleting file ',A)
               LMSG = LENOCC(ERRMSG)
               GOTO 100
            ENDIF
 
   70    CONTINUE
 
   80 CONTINUE
*
*     Wait for some action...
*
      GOTO 10
 
   90 CALL CZCLOS(ISTAT)
      STOP
  100 CONTINUE
*
*     Error exit
*
      IF(IDEBUG.GE.-3) PRINT *,ERRMSG(1:LMSG)
      CALL VMCMS('EXEC TELL JAMIE '//ERRMSG(1:LMSG),IC)
      CALL VMCMS('EXEC TELL JAMIE Logging off...',IC)
      CALL VMCMS('EXEC TELL FATONE Logging off due to'//ERRMSG(1:LMSG),
     +           IC)
      CALL VMSTAK('LOGOFF','L',IC)
      GOTO 90
      END
