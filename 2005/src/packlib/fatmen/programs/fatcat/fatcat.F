*
* $Id: fatcat.F,v 1.1.1.1 1996/03/07 15:17:37 mclareni Exp $
*
* $Log: fatcat.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:37  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_IBMVM)
      PROGRAM FATCAT
*CMZ :          21/02/91  16.24.17  by  Jamie Shiers
*-- Author :    Jamie Shiers   21/02/91
*     Program to move updates between CERNVM and FATCAT
*
      PARAMETER     (NDIR=100)
      CHARACTER*255 CHDIRS(NDIR)
      PARAMETER     (NMAX=500)
      CHARACTER*64  FILES(NMAX)
      CHARACTER*8   FATUSR,FATNOD,REMUSR,REMNOD
      CHARACTER*64  REMOTE,TARGET
      CHARACTER*12  CHTIME
      CHARACTER*8   CHUSER,CHPASS
      CHARACTER*8   CHNODE,CHTYPE,CHSYS,CHRAND
      CHARACTER*6   CHENT
      CHARACTER*80  CHMAIL,LINE,CHDIR
      CHARACTER*38  VALID
      CHARACTER*255 ERRMSG
      COMMON/PAWC/PAW(50000)
      PARAMETER     (IPRINT=6)
      PARAMETER     (IDEBUG=0)
      PARAMETER     (LUNI=1)
      PARAMETER     (LUNO=2)
      INTEGER       FMHOST
#include "zebra/quest.inc"
      COMMON/SLATE/IS(6),IDUMM(34)
      DATA          NENTRY/0/
      DATA          VALID/'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890._'/
*
*     Counters
*
      NSENT = 0
      NRECV = 0
*
*     Warning message for RDR backlog?
*
      IWARN = 0
*
*     Initialise ZEBRA
*
      CALL HLIMIT(50000)
*
*     Initialise XZ
*
      CALL XZINIT(IPRINT,IDEBUG,LUNI,LUNO)
*
      IC = FMHOST(CHNODE,CHTYPE,CHSYS)
      LNODE = LENOCC(CHNODE)
*
*     Open connection to FATCAT...
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
 
      CALL CZOPEN('zserv','FATCAT',IRC)
*
*     First entry: look on fatcat before sleeping
*
      NDIRS = 0
      GOTO 20
 
   10 CALL VMCMS('EXEC FATSERV',IRC)
      IF(IRC.EQ.99) GOTO 20
      IF(IRC.NE.0) THEN
         PRINT 9001,IRC
 9001 FORMAT(' FATCAT. error ',I10,' from FATSERV. Stopping...')
         GOTO 80
      ENDIF
 
      NENTRY = NENTRY + 1
*
*     Get the user and node name for this file...
*
      CALL VMCMS('GLOBALV SELECT *EXEC STACK FATADDR',IC)
      CALL VMRTRM(LINE,IEND)
      ISTART = ICFNBL(LINE,1,IEND)
      CALL FMWORD(FATUSR,0,' ',LINE(ISTART:IEND),IC)
      LFAT   = LENOCC(FATUSR)
      CALL FMWORD(FATNOD,1,' ',LINE(ISTART:IEND),IC)
      LNOD   = LENOCC(FATNOD)
 
      IF(IDEBUG.GE.1) PRINT 9002,FATUSR(1:LFAT),FATNOD(1:LNOD)
 9002 FORMAT(' FATCAT. Update received from ',A, ' at ',A)
*
*     Number of pending files
*
      CALL VMCMS('GLOBALV SELECT *EXEC STACK FATFILES',IC)
      CALL VMRTRM(LINE,IEND)
      NRDR = ICDECI(LINE,1,IEND)
 
      CALL DATIME(ID,IT)
      WRITE(CHTIME,'(I6.6,I4.4,I2.2)') ID,IT,IS(6)
      WRITE(CHENT ,'(I6.6)') NENTRY
      CALL FMRAND(CHRAND,IRC)
*
*    Now put this file...
*    This assumes the FATCAT naming convention: /fatmen/fmgroup,
*                                          e.g. /fatmen/fml3
      CHDIR  = '/fatmen/'//FATUSR(1:LFAT)//
     +         '/todo'
      LDIR   = LENOCC(CHDIR)
*
      REMOTE = ' '
      REMOTE = 'zz'//CHTIME//CHRAND//CHENT
     +         //'.'//FATUSR(1:LFAT)//'_'//FATNOD(1:LNOD)
      LREM   = LENOCC(REMOTE)
      TARGET = REMOTE(1:LREM)
*
*     Change remote directory
*
      CALL CUTOL(CHDIR(1:LDIR))
      IF(IDEBUG.GE.1) PRINT 9003,CHDIR(1:LDIR)
 9003 FORMAT(' FATCAT. Changing remote directory to ',A)
      CALL XZCD(CHDIR(1:LDIR),IRC)
         IF(IRC.NE.0) THEN
            WRITE(ERRMSG,9004) CHDIR(1:LDIR)
 9004 FORMAT(' FATCAT. cannot set directory to ',A)
            LMSG = LENOCC(ERRMSG)
            GOTO 90
         ENDIF
 
      NSENT = NSENT + 1
      IF(IDEBUG.GE.1) PRINT 9005,NSENT,REMOTE(1:LREM)
 9005 FORMAT(' FATCAT. Sending file # ',I6,' as ',A)
      CALL XZPUTA('FATMEN.RDRFILE.A',REMOTE(1:LREM),' ',IC)
      IF(IC.NE.0) THEN
         WRITE(ERRMSG,9006) IC,FATUSR,FATNOD
 9006    FORMAT(' FATCAT. error ',I6,' sending update from ',
     +            A,' at ',A,' to FATCAT')
         LMSG = LENOCC(ERRMSG)
         GOTO 90
      ENDIF
*
*     Rename the remote update file
*
      LSTA = INDEXB(TARGET(1:LREM),'/') + 1
      TARGET(LSTA:LSTA+1) = 'aa'
      IF(IDEBUG.GE.1) PRINT 9007,TARGET(1:LREM)
 9007 FORMAT(' FATCAT. Renaming file to ',A)
      CALL XZMV(REMOTE(1:LREM),TARGET(1:LREM),' ',IRC)
      IF(IRC.NE.0) THEN
         WRITE(ERRMSG,9008) IRC,REMOTE(1:LREM),TARGET(1:LREM)
 9008    FORMAT(' FATCAT. error ',I6,' renaming update from ',
     +            A,' to ',A)
         LMSG = LENOCC(ERRMSG)
         GOTO 90
      ENDIF
*
*     Delete this update...
*
      CALL VMCMS('ERASE FATMEN RDRFILE A',IC)
*
*     Try to clear out RDR
*
      IF(NRDR.GT.10) THEN
         IF(IWARN.EQ.0) WRITE(6,9009) NRDR
 9009 FORMAT(' FATCAT. backlog of ',I6,' files in RDR')
         IWARN = 1
         GOTO 10
      ELSE
         IWARN = 0
      ENDIF
*
*     Are there any files for us to get?
*
   20 CONTINUE
*
*     Get list of remote directories
*
      JCONT  = 0
      IF(NDIRS.EQ.0) THEN
         IF(IDEBUG.GE.1) PRINT 9010
 9010 FORMAT(' FATCAT. Retrieving list of remote directories...')
         CALL XZLS('/fatmen/fm*/tovm',CHDIRS,NDIR,NDIRS,JCONT,'D',IC)
         NDIRS = MIN(NDIR,NDIRS)
         IF(JCONT.NE.0) THEN
            IC = 0
            PRINT 9011
 9011 FORMAT(' FATCAT. too many directories - excess names ',
     +      'will be flushed')
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
*
         ENDIF
      ENDIF
 
      DO 70 J=1,NDIRS
 
         LDIR = LENOCC(CHDIRS(J))
         IF(LDIR.EQ.0) GOTO 70
         CALL CLTOU(CHDIRS(J)(1:LDIR))
*
*     Get the name of the server for whom these updates are intended...
*
         JSTART = INDEX(CHDIRS(J)(1:LDIR),'/FM')
         IF(JSTART.EQ.0) THEN
            IF(IDEBUG.GE.-3) PRINT 9012,CHDIRS(J)(1:LDIR)
 9012 FORMAT(' FATCAT. unrecognised directory - skipped (',A,')')
            GOTO 70
         ELSE
            JSTART = JSTART + 1
         ENDIF
 
         JEND = INDEX(CHDIRS(J)(JSTART:LDIR),'/')
 
         IF(JEND.EQ.0) THEN
            PRINT 9013,CHDIRS(J)(1:LDIR)
 9013 FORMAT(' FATCAT. unrecognised file name - skipped (',A,')')
            GOTO 70
         ENDIF
 
         REMUSR = CHDIRS(J)(JSTART:JSTART+JEND-2)
         LREM = LENOCC(REMUSR)
 
         IF(LREM.EQ.0) THEN
            IF(IDEBUG.GE.-3) PRINT 9013,CHDIRS(J)(1:LDIR)
            GOTO 70
         ENDIF
 
         IF(IDEBUG.GE.1) PRINT 9014,REMUSR(1:LREM)
 9014 FORMAT(' FATCAT. processing updates for ',A)
         CALL XZCD(CHDIRS(J)(1:LDIR),IRC)
         IF(IRC.NE.0) THEN
            WRITE(ERRMSG,9004) CHDIRS(J)(1:LDIR)
            LMSG = LENOCC(ERRMSG)
            GOTO 90
         ENDIF
 
         ICONT  = 0
         NFILES = 0
         IF(IDEBUG.GE.1) PRINT 9015,CHDIRS(J)(1:LDIR)
 9015 FORMAT(' FATCAT. Retrieving list of remote files in ',A)
         CALL XZLS(' ',FILES,NMAX,NFILES,ICONT,' ',IC)
         NFILES = MIN(NFILES,NMAX)
         IF(IDEBUG.GE.2) PRINT 9016,NFILES,CHDIRS(J)(1:LDIR)
 9016 FORMAT(' FATCAT. ',I10,' files found in ',A)
         IF(ICONT.NE.0) THEN
            IC = 0
            IF(IDEBUG.GE.0) PRINT 9017
 9017 FORMAT(' FATCAT. too many files - excess names will be flushed')
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
 
 
         DO 60 I=1,NFILES
            LF = LENOCC(FILES(I))
            IF(LF.EQ.0) GOTO 60
            CALL CLTOU(FILES(I))
*
*     Fix for the case when there are no files...
*
            IF(NFILES.EQ.1) THEN
 
               IF(INDEX(FILES(I)(1:LF),'DOES NOT EXIST').NE.0.OR.
     +         INDEX(FILES (I)(1:LF),'NOT FOUND').NE.0) GOTO 10
 
               IF(INDEX(FILES(I)(1:LF),'ARG LIST TOO LONG').NE.0) THEN
                  IF(IDEBUG.GE.-3) THEN
                     PRINT 9018,FILES(I)(1:LF)
 9018 FORMAT(' FATCAT. Stopping due to the following error...',/,1X,A)
                     PRINT 9019
 9019 FORMAT(' (Intervention required on FATCAT)')
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
                     PRINT 9020,L,FILES(I)(1:LF)
 9020 FORMAT(' FATCAT. invalid character at ',I3,' in ',A)
                     PRINT 9021
 9021 FORMAT(' FATCAT. skipping update...')
                  ENDIF
                  GOTO 60
               ENDIF
   50       CONTINUE
 
            IF(INDEX(FILES(I)(1:LF),CHNODE(1:LNODE)).NE.0) THEN
               IF(IDEBUG.GE.1)
     +         PRINT 9022,CHNODE(1:LNODE),FILES(I)(1:LF)
 9022 FORMAT(' FATCAT. skipping update for ',A,' (',A,')')
               GOTO 60
            ENDIF
            LSLASH = INDEXB(FILES(I)(1:LF),'/')
            IF(FILES(I)(LSLASH+1:LSLASH+2).EQ.'ZZ') THEN
               IF(IDEBUG.GE.1)
     +         PRINT 9023,FILES(I)(1:LF)
 9023 FORMAT(' FATCAT. active file - skipped (',A,')')
               GOTO 60
            ENDIF
 
            IF(IDEBUG.GE.2)
     +      PRINT 9024,REMUSR(1:LREM),FILES(I)(1:LF)
 9024 FORMAT(' FATCAT. update found for ',A,' (',A,')')
 
            NRECV = NRECV + 1
            IF(IDEBUG.GE.1) PRINT 9025,NRECV,FILES(I)(1:LF)
 9025 FORMAT(' FATCAT. retrieving update # ',I10,' (',A,')')
            CALL XZGETA('FATMEN.UPDATE.B',FILES(I)(1:LF),' ',IC)
            IF(IC.NE.0) THEN
               WRITE(ERRMSG,9026) IC,REMUSR(1:LREM)
 9026    FORMAT(' FATCAT. error ',I6,' retrieving update for ',A)
               LMSG = LENOCC(ERRMSG)
               GOTO 90
            ENDIF
 
            CALL VMCMS('EXEC SENDFILE FATMEN UPDATE B TO ' //REMUSR(1:
     +      LREM),IC)
            IF(IC.NE.0) THEN
               WRITE(ERRMSG,9027) IC,REMUSR(1:LREM)
 9027    FORMAT(' FATCAT. error ',I6,' sending update to ',A)
               LMSG = LENOCC(ERRMSG)
               GOTO 90
            ENDIF
 
            CALL XZRM(FILES(I)(1:LF),IC)
            IF(IC.NE.0) THEN
               WRITE(ERRMSG,9028) IC,FILES(I)(1:LF)
 9028    FORMAT(' FATCAT. error ',I6,' deleting file ',A)
               LMSG = LENOCC(ERRMSG)
               GOTO 90
            ENDIF
 
   60    CONTINUE
 
   70 CONTINUE
*
*     Wait for some action...
*
      GOTO 10
 
   80 CALL CZCLOS(ISTAT)
      IF(IDEBUG.GE.-3) PRINT 9030,NSENT,NRECV
      STOP
   90 CONTINUE
*
*     Error exit
*
      IF(IDEBUG.GE.-3) PRINT 9029,ERRMSG(1:LMSG)
 9029 FORMAT(1X,A)
      IF(IDEBUG.GE.-3) PRINT 9030,NSENT,NRECV
 9030 FORMAT(' FATCAT. sent ',I10,' files and received ',I10)
      CALL VMCMS('EXEC TELL JAMIE '//ERRMSG(1:LMSG),IC)
      CALL VMCMS('EXEC TELL JAMIE Logging off...',IC)
      CALL VMCMS('EXEC TELL FATONE Logging off due to'//ERRMSG(1:LMSG),
     +           IC)
      CALL VMSTAK('LOGOFF','L',IC)
      GOTO 80
      END
 
#endif
