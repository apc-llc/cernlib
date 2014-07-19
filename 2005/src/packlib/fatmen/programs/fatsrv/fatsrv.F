*
* $Id: fatsrv.F,v 1.4 2000/06/09 17:03:22 cernsoft Exp $
*
* $Log: fatsrv.F,v $
* Revision 1.4  2000/06/09 17:03:22  cernsoft
* CERNLIB_BIGRZDIR for large memory needed by L3
*
* Revision 1.3  1997/03/17 12:48:05  jamie
* bigger store
*
* Revision 1.2  1996/06/21 12:16:03  jamie
* check for bad FZ files in Unix server
*
* Revision 1.1.1.1  1996/03/07 15:17:40  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PROGRAM FATSRV
*----------------------------------------------------------------------*
*                                                                      *
* MAIN routine for FATMEN Service routine                              *
* FATSRV wakes up at regulard intervals, or upon arrival of new RDR    *
* files (VM), reads them in and updates RZ file.                       *
* ORACLE/SQL database is also updated if +USE,*SQL.                    *
*                                                                      *
* VM/CMS                                                               *
* ------                                                               *
* Issues WAKEUP (via EXEC) to wait for new RDR files in FZ format      *
*                                                                      *
* MVS                                                                  *
* ---                                                                  *
* Read FATSYS, FATGRP, FMWAKEUP and FMLOGL from unit 5.                *
* Look for new FZ files in .todo                                       *
* Use PARM instead of GETENVF, e.g.                                    *
*                                                                      *
* // EXEC PGM=FATSRV,PARM='DESY,H1,60,3'                               *
*                                                                      *
* VAX/VMS                                                              *
* -------                                                              *
* New FZ files are found in <.TODO>. On wakeup, server reads in        *
* FZ files and updates RZ file. FZ files then spooled to CERNVM.       *
* If link to CERNVM is done, renames files to <.TOVM>, else            *
* to <.DONE>. (Files in <.DONE> should be purged by a CRON job).       *
*                                                                      *
* UNIX                                                                 *
* ----                                                                 *
* As for VMS, replacing <.DONE> etc. by /done.                         *
* to /done  . (Files in /done   should be purged by a CRON job).       *
*                                                                      *
*----------------------------------------------------------------------*
#include "fatmen/fatsys.inc"
#include "fatmen/fatout.inc"
#include "fatmen/slate.inc"
#include "fatmen/fatstat.inc"
      INTEGER SYSTEMF
      LOGICAL EXIST
      CHARACTER*8 CHWAKE
#if defined(CERNLIB_VAXVMS)
      INTEGER STATUS
      INCLUDE '($RMSDEF)'
      INTEGER SYS$GETMSG
      PARAMETER (MINBLK=500)
#endif
#if defined(CERNLIB_IBMMVS)
      PARAMETER    (MAXDSN=100)
      CHARACTER*44  DSN(MAXDSN),NEWDSN
      DIMENSION     HDSN(14)
      CHARACTER*100 CHPARM
#endif
      CHARACTER*80  CHLINE
      CHARACTER*255 CHDIR
      CHARACTER*512 COMAND
      CHARACTER*240 GENEN
      CHARACTER*132 LINE,FILEN,NEWF
      CHARACTER*132 TODO
      CHARACTER*255 FNAME
      CHARACTER*255 FATRZ
      CHARACTER*1   DELM
      CHARACTER*4   CHOPT
*     COMMON / /   FENCDB(22), LQ(150000)
#if defined(CERNLIB_BIGRZDIR)
      PARAMETER (LURCOR=2000000)  ! As L3 have one very large directory
#endif
#if !defined(CERNLIB_BIGRZDIR)
      PARAMETER (LURCOR=1000000)
#endif
*     PARAMETER (LURCOR=500000)
*     PARAMETER (LURCOR=80000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
#include "zebra/quest.inc"
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
      INTEGER FMHOST,FMUSER,FAFNDF
      CHARACTER*12   CHUSER,CHSYS,CHLOG
      COMMON/FSRVCM/ HNAME,HTYPE,HSYS
      CHARACTER*8    HNAME,HTYPE,HSYS
      CHARACTER*64   FUNAM
#if defined(CERNLIB_IBMMVS)
      LOGICAL        ITOVM,IDONE
#endif
#if !defined(CERNLIB_IBMMVS)
      LOGICAL        ITOVM,IDONE,ITODO
#endif
*
#include "fatmen/fatbug.inc"
      LUNRZ  = 1
      LUFZFA = 2
*
*
*     Initialise ZEBRA
*
      CALL MZEBRA(-3)
      CALL MZSTOR(IXSTOR,'/CRZT/','Q',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(LURCOR))
 
      CALL MZLOGL(IXSTOR,-3)
*
* *** Define user division and link area like:
*
      CALL MZDIV  (IXSTOR, IXDIV, 'USERS', 50000, LURCOR, 'L')
      CALL MZLINK (IXSTOR, '/USRLNK/', LUSRK1, LUSRLS, LUSRK1)
*
*
*     Determine if we are running on CERNVM...
*
      IC = FMHOST(HNAME,HTYPE,HSYS)
#if defined(CERNLIB_IBMMVS)
      CALL GOPARM(LPARM,CHPARM)
      IF(LPARM.EQ.0) THEN
         NPARMS = 0
      ELSE
         CALL FMNWRD(',',CHPARM(1:LPARM),NPARMS)
      ENDIF
      IF(NPARMS.GT.0) THEN
         CALL FMWORD(CHSYS,0,',',CHPARM(1:LPARM),IRC)
      ELSE
         PRINT *,'FATSRV. GOPARM variable FATSYS not defined. '
         PRINT *,'FATSRV. defaulted to CERN'
         CHSYS = 'CERN'
      ENDIF
#endif
#if defined(CERNLIB_IBMVM)
      CALL VMREXX('F','FATSYS',CHSYS,IRC)
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      CALL GETENVF('FATSYS',CHSYS)
      LDEF = IS(1)
      IRC = 0
      IF(LDEF.EQ.0) IRC = -1
#endif
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
      IF(IRC.NE.0) THEN
         CHSYS = 'CERN'
#endif
#if defined(CERNLIB_IBMVM)
         PRINT *,'FATSRV. REXX variable FATSYS not defined. '
#endif
#if defined(CERNLIB_VAXVMS)
         PRINT *,'FATSRV. symbol FATSYS not defined.'
#endif
#if defined(CERNLIB_UNIX)
         PRINT *,'FATSRV. environmental variable FATSYS not defined. '
#endif
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
         PRINT *,'FATSRV. defaulted to CERN'
      ENDIF
#endif
      CALL CLTOU(CHSYS)
      TOPDIR = '//'//CHSYS
#if defined(CERNLIB_IBMVM)
*
*     Take username from REXX variable 'FATMAN'
*     If not defined, use current username
*     This username determines the FATMEN group for whom we are working...
*
      CALL VMREXX('F','FATMAN',CHUSER,IRC)
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      CALL GETENVF('FATGRP',CHUSER)
      LDEF = IS(1)
      IRC = 0
      IF(LDEF.EQ.0) IRC = -1
#endif
#if defined(CERNLIB_IBMMVS)
      IF(NPARMS.GE.2) THEN
         CALL FMWORD(CHUSER,1,',',CHPARM(1:LPARM),IRC)
      ELSE
         PRINT *,'FATSRV. GOPARM variable FATUSER not defined. '
         IRC = -1
      ENDIF
#endif
      IF(IRC.NE.0) THEN
         IC = FMUSER(CHUSER)
#if defined(CERNLIB_IBMVM)
         PRINT *,'FATSRV. REXX variable FATMAN not defined. '
#endif
#if defined(CERNLIB_VAXVMS)
         PRINT *,'FATSRV. symbol FATGRP not defined.'
#endif
#if defined(CERNLIB_UNIX)
         PRINT *,'FATSRV. environmental variable FATGRP not defined. '
#endif
         PRINT *,'Using current username'
      ENDIF
      CALL CLTOU(CHUSER)
 
      IF(CHUSER(1:2).NE.'FM') THEN
        SERNAM = 'FM' // CHUSER(1:LENOCC(CHUSER))
        ELSE
        SERNAM = CHUSER(1:LENOCC(CHUSER))
        ENDIF
      LSN    = LENOCC(SERNAM)
*
*
*     Get directory where RZ file is kept...
*
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      DEFAULT = ' '
      CALL GETENVF(SERNAM(1:LSN),DEFAULT)
      LDEF = IS(1)
      IF(LDEF.GT.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FATSRV. catalogue is in directory ',
     +      DEFAULT(1:LDEF)
      ELSE
         PRINT *,'FATSRV. symbol/variable ',SERNAM(1:LSN),
     +      ' is undefined. Using current directory'
         CALL GETWDF(DEFAULT)
         LDEF = IS(1)
      ENDIF
#endif
#if defined(CERNLIB_IBMMVS)
*
*     We are abit less flexible than on other systems and
*     require a DD card for the FATMEN RZ file...
*
      CALL VBLANK(HDSN,14)
      CALL FTINFO(LUNRZ,-1,HDSN,IRC)
      CALL UHTOC(HDSN,4,DEFAULT,LEN(DEFAULT))
      LDEF = LENOCC(DEFAULT)
      LDEF = INDEXB(DEFAULT(1:LDEF),'.') - 1
      LDEF = INDEXB(DEFAULT(1:LDEF),'.') - 1
      DEFAULT(LDEF+1:) = ' '
#endif
*
*     Get wakeup interval
*
      ISLEEP = 60
#if defined(CERNLIB_IBMMVS)
      IF(NPARMS.GE.3) THEN
         CALL FMWORD(CHWAKE,2,',',CHPARM(1:LPARM),IRC)
      ELSE
         IC = -1
      ENDIF
#endif
#if defined(CERNLIB_IBMVM)
      CALL VMREXX('F','FMWAKEUP',CHWAKE,IC)
      LCHW  = LENOCC(CHWAKE)
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      CALL GETENVF('FMWAKEUP',CHWAKE)
      LCHW  = IS(1)
      IF(LCHW.GT.0) THEN
         IC = 0
      ELSE
         IC = -1
      ENDIF
#endif
      IF(IC.EQ.0) THEN
         ISLEEP = ICDECI(CHWAKE,1,8)
      ENDIF
      IF(ISLEEP.EQ.0) THEN
         ISLEEP = 60
         WRITE(CHWAKE,'(I8.8)') ISLEEP
      ENDIF
*
      PRINT *,'FATSRV. Initialising for group ',CHUSER(1:LSN)
      PRINT *,'FATSRV. wakeup interval is ',ISLEEP,' seconds'
#if defined(CERNLIB_VAX)
      SLEEP = FLOAT(ISLEEP)
#endif
*
*     Set logging level
*
#if defined(CERNLIB_IBMVM)
      CALL VMREXX('F','FMLOGL',CHLOG,IRC)
      IF(IRC.EQ.0) ILOG = ICDECI(CHLOG,1,8)
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      CALL GETENVF('FMLOGL',CHLOG)
      IF(IS(1).GT.0) ILOG = ICDECI(CHLOG,1,8)
#endif
#if defined(CERNLIB_IBMMVS)
      IF(NPARMS.GE.4) THEN
         CALL FMWORD(CHLOG,3,',',CHPARM(1:LPARM),IRC)
         IF(IRC.EQ.0) ILOG = ICDECI(CHLOG,1,8)
      ENDIF
#endif
*
*     Initialise FATMEN...
*
      CALL FMLOGL(ILOG)
*
*     Set Zebra loglevel
*
      IZLOG = MAX(ILOG,-3)
      IZLOG = MIN(ILOG,2)
      CALL MZLOGL(IXSTOR,IZLOG)
*
*     Force non-CSPACK access
*
      FATNOD = ' '
*
*     Single user mode
*
      LTOP = LENOCC(TOPDIR)
      CHOPT = '1DL '
#if defined(CERNLIB_UNIX)
      CHOPT = '1DLX'
#endif
      CALL FATINI(IXSTOR,LUNRZ,LUFZFA,TOPDIR(1:LTOP),CHOPT)
      OUTPUT = 'TTY'
*
*     Zero counters
*
      NFMUSR = 0
      NFMNOD = 0
      NFMFIL = 0
      NFMCOM = 0
      NFMPUT = 0
      NFMMOD = 0
      NFMDEL = 0
      NFMMDR = 0
      NFMRMD = 0
      NFMOPN = 0
      NFMCLS = 0
      NFMPDK = 0
      NFMPRP = 0
      NFMPDL = 0
      NFMBAD = 0
      NFMLOG = 0
*
*     Compare bit map of allocated records against the file itself
*
      CALL RZVERI(TOPDIR(1:LTOP),'C')
      IF(IQUEST(2).NE.0) THEN
*
*     Rebuild bit map
*
         PRINT *,'FATSRV. ',IQUEST(2),
     +           ' records marked as free but in use'
         PRINT *,'FATSRV. rebuilding bitmap...'
         CALL RZFREE('RZFILE')
         CALL RZVERI(TOPDIR(1:LTOP),'B')
         CALL RZLOCK('RZFILE')
      ENDIF
*
*     Print version
*
      CALL FMVERS
#if (defined(CERNLIB_APOLLO))&&(defined(CERNLIB_PLONK))
      NRETRY = 0
   10 CONTINUE
      NRETRY = NRETRY + 1
      CALL RZEND(TOPDIR(3:LTOP))
      CLOSE (LUNRZ)
*
*     Single user mode
*
      FATRZ = TOPDIR(3:LTOP)//'.FATRZ'
      LRECL = 0
      CALL FAOPEN('FARZ',LUNRZ,'U',FATRZ,LRECL,5000)
      IF(IFMODX.EQ.0) THEN
         CHOPT = 'UDL'
      ELSE
         CHOPT = 'UDLX'
      ENDIF
      CALL RZFILE(LUNRZ,TOPDIR(3:LTOP),CHOPT)
      IF(IQUEST(1).NE.0) THEN
         PRINT *,'FATSRV. error ',IQUEST(1),' from RZFILE'
         IF(NRETRY.LT.2) THEN
            CALL RZEND(TOPDIR(3:LTOP))
            CLOSE (LUNRZ)
            GOTO 10
         ELSE
            PRINT *,'giving up after ',NRETRY,' retries'
            STOP
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_SQLCOM)
*
*     Connect to ORACLE or SQL/DS
*
      DO 20 I=1,5
      CALL FMLOGI(IRC)
      IF(IRC.EQ.0) GOTO 30
      PRINT *,'FATSRV. cannot connect to ORACLE.',
     +        ' Will sleep for 1 minute then retry'
      CALL SLEEPF(60)
   20 CONTINUE
*
*     Cannot connect to ORACLE after 5 attempts - shutdown
*
         CALL RZEND(TOPDIR(3:LTOP))
         CLOSE (LUNRZ)
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_SQLCOM))
         CALL VMCMS('#CP LOGOFF',IRC)
#endif
#if defined(CERNLIB_SQLCOM)
         STOP
 
   30 CONTINUE
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
*
*     Find directory name where updates are kept
*
      TODO  = ' '
#endif
#if defined(CERNLIB_UNIX)
      TODO  = DEFAULT(1:LDEF) // '/todo'
#endif
#if defined(CERNLIB_VAXVMS)
      TODO  = DEFAULT(1:LDEF-1) // '.TODO]AA*.*'
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      LTODO = LENOCC(TODO)
#endif
*
*     Check that TODO directory exists
*
#if defined(CERNLIB_UNIX)
      INQUIRE(FILE=TODO(1:LTODO),EXIST=ITODO)
      IF(.NOT.ITODO) THEN
         PRINT *,'FATSRV. directory ',TODO(1:LTODO),
     +           ' does not exist. Stopping...'
         GOTO 100
      ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
      INQUIRE(FILE=DEFAULT(1:LDEF)//'TODO.DIR',EXIST=ITODO)
      IF(.NOT.ITODO) THEN
         PRINT *,'FATSRV. directory ',TODO(1:LTODO-5),
     +           ' does not exist. Stopping...'
         GOTO 100
      ENDIF
#endif
*
*     Do we need to copy to VM?
*
#if defined(CERNLIB_IBMMVS)
      INQUIRE(FILE='/'//DEFAULT(1:LDEF)//'.tovm',EXIST=ITOVM)
#endif
#if defined(CERNLIB_VAXVMS)
      INQUIRE(FILE=DEFAULT(1:LDEF)//'TOVM.DIR',EXIST=ITOVM)
#endif
#if defined(CERNLIB_UNIX)
      INQUIRE(FILE=DEFAULT(1:LDEF)//'/tovm',EXIST=ITOVM)
#endif
      IF(ITOVM) THEN
         PRINT *,'FATSRV. tovm directory exists - ',
     +           'will attempt to send files to CERNVM.'
      ENDIF
*
*     Check that DONE directory exists
*
#if defined(CERNLIB_VAXVMS)
      INQUIRE(FILE=DEFAULT(1:LDEF)//'DONE.DIR',EXIST=IDONE)
      IF(.NOT.IDONE) THEN
         CHDIR = DEFAULT(1:LDEF-1)//'.DONE'//DEFAULT(LDEF:LDEF)
         LDIR  = LENOCC(CHDIR)
         IF(IDEBFA.GE.0) PRINT *,'FATSRV. creating directory ',
     +      CHDIR(1:LDIR)
         IC = LIB$CREATE_DIR(CHDIR(1:LDIR))
         IF(.NOT.IC) CALL LIB$SIGNAL(%VAL(IC))
      ENDIF
#endif
#if defined(CERNLIB_UNIX)
      INQUIRE(FILE=DEFAULT(1:LDEF)//'/done',EXIST=IDONE)
      IF(.NOT.IDONE) THEN
         CHDIR = DEFAULT(1:LDEF)//'/done'
         LDIR  = LENOCC(CHDIR)
         CALL CUTOL(CHDIR(1:LDIR))
         IF(IDEBFA.GE.0) PRINT *,'FATSRV. creating directory ',
     +      CHDIR(1:LDIR)
         IC    = SYSTEMF('mkdir '//CHDIR(1:LDIR))
         IF(IC.NE.0) PRINT *,'FATSRV. error creating directory'
      ENDIF
#endif
*
*     Infinite loop - wait for new RDR files and then try to
*     read them in with FZ
*
   40 CONTINUE
#if defined(CERNLIB_IBMVM)
      CALL VMCMS('EXEC FATSERV',IC)
      IF((IC.EQ.3).OR.(IC.EQ.99)) GOTO 40
      IF(IC.NE.0) THEN
         PRINT *,'ERROR FROM VMCMS, CODE=',IC
         GOTO 100
      ENDIF
*
*     If we get here, we should have just received a file
*     Try to read in with FZIN
*
      CALL FMFZI(' ',GENEN,ICOUNT)
*
*     Error opening input file
*
      IF(IQUEST(1).NE.0) GOTO 40
      LG = LENOCC(GENEN)
*
*     No updates found in input file (curious)
*
      IF(ICOUNT.EQ.0) THEN
 
         IF(IDEBFA.GE.-1) WRITE(LPRTFA,9002)
 
      ELSE
*
*     Forward updates to remote nodes
*     (we now require an entry in the names file also for CERNVM)
*
         CALL VMCMS('EXEC FAT4WARD '//GENEN(1:LG),IRC)
 
      ENDIF
*
*     Store journal file
*
      CALL VMCMS('EXEC FATJOURN',IRC)
*
*     Close and re-open RZ file so that updates are visible
*
      CALL RZEND(TOPDIR(3:LTOP))
      CLOSE (LUNRZ)
*
*     Single user mode
*
      FATRZ = TOPDIR(3:LTOP)//'.FATRZ.A6'
      LRECL = 0
      CALL FAOPEN('FARZ',LUNRZ,'U',FATRZ,LRECL,5000)
      IF(IFMODX.EQ.0) THEN
         CHOPT = '1'
      ELSE
         CHOPT = '1X'
      ENDIF
      CALL RZFILE(LUNRZ,TOPDIR(3:LTOP),CHOPT)
 
      GOTO 40
#endif
#if defined(CERNLIB_IBMMVS)
*
*     Look for new files in .todo
*
   50 CONTINUE
      CALL FALCAT(DEFAULT(1:LDEF)//'.TODO',DSN,MAXDSN,NUMDSN,IRC)
      IF((IRC.EQ.0).AND.(NUMDSN.EQ.0)) THEN
         IF(IDEBFA.GE.1) THEN
            CALL DATIME(ID,IT)
            PRINT *,'FATSRV. time is ',ID,IT,
     +      ' sleeping for ',ISLEEP,' seconds'
         ENDIF
         CALL SLEEPF(ISLEEP)
         GOTO 40
      ELSE
         DO 70 I=1,NUMDSN
*
*     Read in FZ file
*
            LDSN = LENOCC(DSN(I))
            IF(INDEX(DSN(I)(1:LDSN),'SIGNAL.STOP').NE.0) GOTO 100
            CALL FMFZI(DSN(I)(1:LDSN),GENEN,ICOUNT)
*
*     Error opening input file
*
            IF(IQUEST(1).NE.0) THEN
               CALL SLEEPF(ISLEEP)
               GOTO 40
            ENDIF
 
            IF(ICOUNT.EQ.0) THEN
 
               IF(IDEBFA.GE.-1) WRITE(LPRTFA,9002)
 
            ELSE
*
*     Queue to remote servers
*
            ITODO  = INDEX(DSN(I)(1:LDSN),'.TODO')
            NEWDSN = DSN(I)(1:LDSN)
            NEWDSN(ITODO:ITODO+4) = '.TOVM'
   60 CONTINUE
            CALL FMFNME(FUNAM)
            NEWDSN(ITODO+5:)      = '.'//FUNAM
            LNEW = LENOCC(NEWDSN)
            INQUIRE(FILE='/'//NEWDSN(1:LNEW),EXIST=EXIST)
            IF(EXIST) THEN
               CALL SLEEPF(1)
               GOTO 60
            ENDIF
            CALL FAMOVE(DSN(I)(1:LDSN),NEWDSN(1:LNEW),IC)
 
            ENDIF
 
   70    CONTINUE
 
*
*     Any more files to be processed?
*
         IF(IRC.EQ.1) GOTO 50
      ENDIF
 
      GOTO 40
#endif
#if defined(CERNLIB_VAXVMS)
*
*     Check disk quota
*
      CALL FMQUOT(IUSED,IFREE,IALLO,IOVER,IRC)
 
      IF(IRC.LT.0.AND.IDEBFA.GE.0) PRINT *,'FATSRV. error obtaining ',
     +   'disk quota information'
 
      IF(IRC.EQ.0.AND.IFREE.LT.MINBLK) THEN
         IF(IDEBFA.GE.0) PRINT *,'FATSRV. triggering disk cleanup'
         IF(IDEBFA.GE.0) PRINT *,'Current usage = ',IUSED,IFREE,
     +      IALLO,IOVER,' blocks used/free/allocated/overdraft'
         IRC = SYSTEMF('PURGE *.*/LOG')
         CALL FMQUOT(IUSED,IFREE,IALLO,IOVER,IRC)
         IF(IDEBFA.GE.0) PRINT *,'After cleanup = ',IUSED,IFREE,
     +      IALLO,IOVER,' blocks used/free/allocated/overdraft'
 
      ENDIF
 
      IF(IDEBFA.GE.3) PRINT *,'FATSRV. looking for files in ',
     +   TODO(1:LTODO)
      ICONT  = 0
 
   80 CONTINUE
*
*     Check for signal.stop file
*
      JCONT  = 0
      STATUS = LIB$FIND_FILE(TODO(1:LTODO-5)//'SIGNAL.STOP',
     +                       FILEN,JCONT)
      ISTAT  = LIB$FIND_FILE_END(JCONT)
      IF(STATUS.EQ.RMS$_SUC) THEN
         PRINT *,'FATSRV. signal.stop file found - stopping'
         GOTO 100
      ENDIF
*
*     Look for new files in <.TODO>
*
      STATUS = LIB$FIND_FILE(TODO(1:LTODO),FILEN,ICONT)
*
*     Unexpected error
*
      IF(STATUS.NE.RMS$_NMF.AND.STATUS.NE.RMS$_FNF.AND.
     +   .NOT.STATUS) THEN
         CALL LIB$SIGNAL(%VAL(STATUS))
      ENDIF
*
*     No more files - clear context pointer
*
      IF(STATUS.EQ.RMS$_NMF) THEN
         IRC   = LIB$FIND_FILE_END(ICONT)
         ICONT = 0
         GOTO 40
      ENDIF
*
*     File not found - sleep
*
      IF(STATUS.EQ.RMS$_FNF) THEN
         IRC   = LIB$FIND_FILE_END(ICONT)
         ICONT = 0
         IF(IDEBFA.GE.1) THEN
            CALL DATIME(ID,IT)
            PRINT *,'FATSRV. time is ',ID,IT,
     +      ' sleeping for ',ISLEEP,' seconds'
         ENDIF
         CALL LIB$WAIT(SLEEP)
         GOTO 40
      ENDIF
 
      LFILEN = INDEX(FILEN,';') -1
      IF(IDEBFA.GE.1) PRINT *,'New FZFILE found ',FILEN(1:LFILEN)
*
*     Read in FZ file
*
      CALL FMFZI(FILEN(1:LFILEN),GENEN,ICOUNT)
*
*     Error opening input file
*
      IF(IQUEST(1).NE.0) THEN
         CALL SLEEPF(ISLEEP)
         GOTO 80
      ENDIF
 
      IF(ICOUNT.EQ.0) THEN
 
         IF(IDEBFA.GE.-1) WRITE(LPRTFA,9002)
 
      ELSE
*
*     Send to remote servers
*
         CALL FMSSND(GENEN,FILEN(1:LFILEN),IRC)
 
      ENDIF
*
*     Try to copy to CERNVM
*
      IF(ITOVM.AND.ICOUNT.GT.0) THEN
         CALL FMTOVM(LUFZFA,FILEN(1:LFILEN),IRC)
      ELSE
         IRC = 0
      ENDIF
 
      IDIR = INDEX(FILEN,'.TODO')
      NEWF = FILEN
 
      IF (IRC .EQ. 0) THEN
*
*     Send to VM successful, can rename to <.DONE>
*
         NEWF(IDIR+1:IDIR+4) = 'DONE'
         LCOLON = INDEX(FILEN(1:LFILEN),';')
         IF(LCOLON.NE.0) LFILEN = LCOLON
         IF(IDEBFA.GE.3) PRINT *,'FATSRV. renaming ',
     +      FILEN(1:LFILEN),' to ',NEWF(1:LFILEN)
         IC = LIB$RENAME_FILE(FILEN(1:LFILEN),NEWF(1:LFILEN),,,,,,,,,,)
         IF (.NOT. IC) CALL LIB$SIGNAL(%VAL(IC))
 
      ELSE
*
*     Send to VM failed, queue to <.TOVM>
*
         NEWF(IDIR+1:IDIR+4) = 'TOVM'
         IF(IDEBFA.GE.3) PRINT *,'FATSRV. renaming ',
     +      FILEN(1:LFILEN),' to ',NEWF(1:LFILEN)
         IC = LIB$RENAME_FILE(FILEN(1:LFILEN),NEWF(1:LFILEN),,,,,,,,,,)
         IF (.NOT. IC) CALL LIB$SIGNAL(%VAL(IC))
 
      ENDIF
*
*     Close and re-open RZ file so that updates are visible
*
      CALL RZEND(TOPDIR(3:LTOP))
      CLOSE (LUNRZ)
*
*     Single user mode
*
      FATRZ = TOPDIR(3:LTOP)//'.FATRZ'
      LRECL = 0
      CALL FAOPEN('FARZ',LUNRZ,'SU',FATRZ,LRECL,5000)
      IF(IFMODX.EQ.0) THEN
         CHOPT = '1DL'
      ELSE
         CHOPT = '1DLX'
      ENDIF
      CALL RZFILE(LUNRZ,TOPDIR(3:LTOP),CHOPT)
*
*     Any more files with current context pointer?
*
      GOTO 80
#endif
#if defined(CERNLIB_UNIX)
 
*
*     Look for new files in /todo
*
 
      ICONT = 0
   90 CONTINUE
      FILEN = ' '
 
      INQUIRE (FILE=TODO(1:LTODO)//'/signal.stop', EXIST=EXIST)
      IF(EXIST) THEN
         PRINT *,'FATSRV. signal.stop file found - stopping'
         GOTO 100
      ENDIF
 
      ISTAT = FAFNDF(TODO(1:LTODO),FILEN,ICONT)
*
*     If we were processing a previous batch of files and hit
*     EOF, go look immediately if there are now some more
*
      IF(ISTAT.EQ.1.AND.ICONT.EQ.1) GOTO 40
*
*     EOF?
*
      IF(ISTAT.NE.0) THEN
         IF(IDEBFA.GE.1) THEN
            CALL DATIME(ID,IT)
            PRINT *,'FATSRV. time is ',ID,IT,
     +      ' sleeping for ',ISLEEP,' seconds'
         ENDIF
         CALL SLEEPF(ISLEEP)
         GOTO 40
      ENDIF
 
      ICONT  = 1
*
*     Ignore files . and ..
*
      IF((FILEN(1:1).EQ.'.')) GOTO 90
      LFILEN = LENOCC(FILEN)
*
*     Ignore Kumacs
*
      IF(INDEX(FILEN(1:LFILEN),'.kumac').NE.0) GOTO 90
 
      IF(IDEBFA.GE.1) PRINT *,'FATSRV. new FZFILE found ',
     +   FILEN(1:LFILEN)
 
      IF (INDEX(FILEN,'signal.stop') .NE. 0) GOTO 100
 
      IF (FILEN(1:2).EQ.'zz'.OR.FILEN(1:2).EQ.'ZZ') THEN
         IF(IDEBFA.GE.1) THEN
            CALL DATIME(ID,IT)
            PRINT *,'FATSRV. time is ',ID,IT,
     +      ' sleeping for ',ISLEEP,' seconds'
         ENDIF
         CALL SLEEPF(ISLEEP)
         GOTO 40
      ENDIF
*
*     Read in FZ file
*
      FNAME = TODO(1:LTODO)//'/'//FILEN(1:LFILEN)
      LFNAM = LENOCC(FNAME)
*
*     Check for bad FZ files
*
      OPEN(LUFZFA,FILE=FNAME(1:LFNAM),STATUS='OLD',ACCESS='SEQUENTIAL',
     +     FORM='FORMATTED')

      READ(LUFZFA,'(A)') CHLINE

      CLOSE(LUFZFA)

      IF(CHLINE(1:2).NE.'>>') THEN
         IF(IDEBFA.GE.0) WRITE(LPRTFA,7001) FNAME(1:LFNAM)
7001  FORMAT(' FATSRV. bad FZ file: ',A, ' moving to BAD directory')
         ISTAT = 
     +SYSTEMF('mv '//FNAME(1:LFNAM)//' '//DEFAULT(1:LDEF)//'/bad')
         GOTO 40
      ENDIF

      CALL FMFZI(FNAME(1:LFNAM),GENEN,ICOUNT)
*
*     Error opening input file
*
      IF(IQUEST(1).NE.0) THEN
         CALL SLEEPF(ISLEEP)
         GOTO 40
      ENDIF
 
      IF(ICOUNT.EQ.0) THEN
 
         IF(IDEBFA.GE.-1) WRITE(LPRTFA,9002)
 
      ELSE
 
*
*     Send to remote servers
*
      CALL FMSSND(GENEN,FNAME(1:LFNAM),IRC)
 
      ENDIF
*
*     Try to copy to CERNVM
*
      IF(ITOVM.AND.ICOUNT.GT.0) THEN
         CALL FMTOVM(LUFZFA,FNAME(1:LFNAM),IRC)
      ELSE
         IRC = 0
      ENDIF
 
      IDIR  = INDEX(TODO,'/todo')
      NEWF  = FNAME
      LNEWF = LFNAM
 
      IF (IRC .EQ. 0) THEN
*
*     Send to VM successful, can rename to /done
*
         NEWF(IDIR+1:IDIR+4) = 'done'
         COMAND = 'mv ' //FNAME(1:LFNAM)// ' ' //NEWF(1:LNEWF)
         LCOM = LENOCC(COMAND)
         ISTAT = SYSTEMF(COMAND(1:LCOM))
         PRINT *,'FATSRV. executing ',COMAND(1:LCOM)
 
      ELSE
*
*     Send to VM failed, queue to /tovm
*
         NEWF(IDIR+1:IDIR+4) = 'tovm'
         COMAND = 'mv ' //FNAME(1:LFNAM)// ' ' //NEWF(1:LNEWF)
         LCOM = LENOCC(COMAND)
         ISTAT = SYSTEMF(COMAND(1:LCOM))
         IF(IDEBFA.GE.3) PRINT *,'FATSRV. executing ',COMAND(1:LCOM)
 
      ENDIF
*
*     Close and re-open RZ file so that updates are visible
*
      CALL RZEND(TOPDIR(3:LTOP))
      CLOSE (LUNRZ)
*
*     Single user mode, exchange mode catalogue
*
      FATRZ = TOPDIR(3:LTOP)//'.FATRZ'
      LRECL = 0
      CALL FAOPEN('FARZ',LUNRZ,'UX',FATRZ,LRECL,5000)
      CHOPT = '1DLX'
      CALL RZFILE(LUNRZ,TOPDIR(3:LTOP),CHOPT)
      IF(IQUEST(1).NE.0) THEN
         PRINT *,'FATSRV. return code ',IQUEST(1),' from RZFILE'
         GOTO 100
      ENDIF
 
      GOTO 90
#endif
 
  100 CALL RZEND(TOPDIR(3:LTOP))
      CLOSE (LUNRZ)
*
*     Print termination statistics
*
      IF(IDEBFA.GE.0) THEN
         WRITE(LPRTFA,9001) NFMUSR,NFMNOD,NFMFIL,NFMCOM
     +,NFMPUT,NFMMOD,NFMDEL,NFMMDR,NFMRMD,NFMOPN,NFMCLS
     +,NFMPDK,NFMPRP,NFMPDL,NFMBAD,NFMLOG
 
 9001 FORMAT(' FATSRV. termination statistics: ',/,
     +       ' Number of different users = ',I6,' (not yet counted) ',/,
     +       ' Number of different nodes = ',I6,' (not yet counted) ',/,
     +       ' Number of files           = ',I6,/,
     +       ' Number of commands        = ',I6,/,
     +       ' Number of PUTs            = ',I6,/,
     +       ' Number of MODs            = ',I6,/,
     +       ' Number of DELs            = ',I6,/,
     +       ' Number of MKDIRs          = ',I6,/,
     +       ' Number of RMDIRs          = ',I6,/,
     +       ' Number of OPENs           = ',I6,/,
     +       ' Number of CLOSEs          = ',I6,' (not yet counted) ',/,
     +       ' Number of RZDELK warnings = ',I6,/,
     +       ' Number of REP failures    = ',I6,' (no such entry)   ',/,
     +       ' Number of DEL failures    = ',I6,' (no such entry)   ',/,
     +       ' Number of BAD commands    = ',I6,' (unrecognised)    ',/,
     +       ' Number of LOG commands    = ',I6)
      ENDIF
 
 9002    FORMAT(' FATSRV. no updates found in journal file - will not',
     +          ' be sent to remote nodes.')
 
      END
