*
* $Id: cdserv.F,v 1.6 1996/04/23 14:05:26 jamie Exp $
*
* $Log: cdserv.F,v $
* Revision 1.6  1996/04/23 14:05:26  jamie
* increase zebra store to 3M words
*
* Revision 1.5  1996/04/12 09:42:27  cernlib
* New handling of title
*
* Revision 1.4  1996/04/02 14:43:09  cernlib
* Split up "/ *" to '/' // '*'; this does fpp on VMS a favour
*
* Revision 1.3  1996/03/29 11:18:21  jamie
* qftitlch
*
* Revision 1.2  1996/03/12 13:08:06  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:35  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CDSERV
*     ==============
*
*     HEPDB database server
*
*     Method:
*     =======
*
*     Upon initialisation, the server reads configuration information
*     from a names file. This determines which database files are to
*     be managed, the directories where updates will be found, the
*     directories where journal files will be saved, the names of
*     remote servers etc.
*
*     The server then looks for files in the update directory. It stops
*     if a file named signal.stop is found. Otherwise, it determines from
*     the update file name the name of the database to be processed,
*     opens the database and processes the update file.
*
*     The update file is then moved to a journal directory and copied
*     to the input directories of all known remote servers. In some
*     cases, the files are first copied to a local queue and moved to
*     the final directories using another server.
*
*     The server can operate in one of two modes:
*
*         1) Master
*            Updates are allocated a unique (within a directory) KEY(1)
*            and stamped with the insertion date and time
*         2) Slave
*
*     The master server is the one where the directories pointed
*     to by the :todo and :queue tags are the same.
*
*     This permits the same code to run as both master and slave
*
#include "hepdb/cdnamc.inc"
#include "hepdb/cduscm.inc"
#include "hepdb/cdwacm.inc"
#include "zebra/fzstat.inc"
#include "hepdb/slate.inc"
      PARAMETER (MAXGCB = 3000000)
      COMMON /GCBANK/ FENCE(22),LQ(MAXGCB)
      DIMENSION IQ(999),Q(999)
      EQUIVALENCE (Q(1),IQ(1),LQ(9))
      PARAMETER (NMAX=100)
      CHARACTER*255 CHFILE,CHSTOP,CHSEAR,FILES(NMAX)
      CHARACTER*80  CHTEXT,CHJOUR
      CHARACTER*4   CHTOP,CHOPT
      CHARACTER*2   PREFIX
      CHARACTER*8   CHNODE
      LOGICAL       ISTOP,IFBATCH
#include "hepdb/quest.inc"
#include "hepdb/qftitlch.inc"
      CHARACTER    VIDQQ*(*)
      PARAMETER   (VIDQQ = '@(#)' //
     + HepdbTitleFortranString
     + // '>')
      CALL CLEFT (VIDQQ,1,0)
*
*-----------------------------------------------------------------------
 
      IOPLOG = 0
      LUNRZ  = 1
      LUNFZ  = 2
      LUNLG  = 3
      LUNNF  = 4
      LPRTCD = 6
      LUNCDI = 7
      LUNJOU = 8
      IWAKCD = 0
 
      NFILES = 0
      CALL VZERO(NSERV,MAXDB)
#if defined(CERNLIB_VAXVMS)
      OPEN(LPRTCD,FILE='SYS$OUTPUT',RECL=512,STATUS='UNKNOWN',
     +     FORM='FORMATTED',ACCESS='SEQUENTIAL')
#endif
*
*     Read configuration from NAMES file
*
      CHFILE = 'HEPDB.NAMES'
      CALL CDCONF(LUNNF,CHFILE,ILOGLV,IWAKE,IRC)
 
      IDEBCD = ILOGLV
 
#if defined(CERNLIB_VAXVMS)
      CHSTOP = CDTODO(1:LTODO) // 'SIGNAL.STOP'
      LSTOP  = LTODO + 11
 
      TMIN   = 5.
      CALL GETENVF('CDTMIN',CHTEXT)
      IF(IS(1).NE.0) TMIN = ICDECI(CHTEXT,1,IS(1))
 
#endif
#if defined(CERNLIB_UNIX)
      CHSTOP = CDTODO(1:LTODO) // '/SIGNAL.STOP'
      LSTOP  = LTODO + 12
      CALL CUTOL(CHSTOP(1:LSTOP))
#endif
#if defined(CERNLIB_IBMMVS)
      CHSTOP = CDTODO(1:LTODO) // '.SIGNAL.STOP'
      LSTOP  = LTODO + 12
#endif
*
***   Initialize ZEBRA
*
#if defined(CERNLIB__DEBUG)
      CALL MZEBRA(0)
#endif
#if !defined(CERNLIB__DEBUG)
      CALL MZEBRA(-3)
#endif
*
      CALL DATIME(ID,IT)
      CALL CDHOST(CHNODE,IRC)
      LNODE = LENOCC(CHNODE)
      CALL CLTOU (CHNODE)
      WRITE(CHTEXT,9010) 'Init. at ',ID,IT,' on node ',CHNODE
      CALL CDLOGF(0,CHTEXT)
#if defined(CERNLIB_UNIX)
      CALL GETPIDF(IPID)
      WRITE(CHTEXT,9001) IPID,CHNODE
 9001 FORMAT('CDSERV. PID = ',I10,' NODE = ',A)
      CALL CDLOGF(0,CHTEXT)
#endif
 
#if defined(CERNLIB__DEBUG)
      CALL MZSTOR(IXSTOR,'/GCBANK/',' ',FENCE(1),LQ(1),LQ(5),LQ(9),
     +                             LQ(5000),LQ(MAXGCB))
      CALL MZLOGL(IXSTOR,0)
#endif
#if !defined(CERNLIB__DEBUG)
      CALL MZSTOR(IXSTOR,'/GCBANK/','Q',FENCE(1),LQ(1),LQ(5),LQ(9),
     +                             LQ(5000),LQ(MAXGCB))
      CALL MZLOGL(IXSTOR,-3)
#endif
      IDIVD = IXSTOR + 2
*
***   Loop waiting for work
*
   10 CONTINUE
*
*     Look for files in the input queue
*
      JWAKCD = IWAKCD
#if !defined(CERNLIB_IBMVM)
      INQUIRE(FILE=CHSTOP(1:LSTOP),EXIST=ISTOP)
      IF(ISTOP) GOTO 40
#endif
#if defined(CERNLIB_VAXVMS)
      IF(IFBATCH(DUMMY)) THEN
         CALL TIMEL(TLEFT)
         IF(TLEFT.LT.TMIN) THEN
            WRITE(CHTEXT,9001) TLEFT,TMIN
 9001 FORMAT('Only ',F10.1,' seconds left. Minimum ',F10.1)
            CALL CDLOGF(0,CHTEXT)
            GOTO 40
         ENDIF
      ENDIF
      CHSEAR = CDTODO(1:LTODO) // '*.*'
#endif
#if defined(CERNLIB_UNIX)
      CHSEAR = CDTODO(1:LTODO) // '/' // '*'
#endif
#if !defined(CERNLIB_IBMVM)
      ICONT  = 0
      CALL CDLLS(LUNCDI,CHSEAR,FILES,NMAX,NFOUND,ICONT,IRC)
      IF(NFOUND.EQ.0) THEN
         IF(IWAKCD.EQ.JWAKCD) THEN
            CALL SLEEPF(IWAKE)
            JWAKCD = IWAKCD
         ENDIF
         GOTO 10
      ENDIF
 
      NZZ    = 0
 
      DO 30 I=1,NFOUND
         CHFILE = FILES(I)
         LFILE = LENOCC(CHFILE)
#endif
#if defined(CERNLIB_VAXVMS)
         LSQBRA = INDEXB(CHFILE(1:LFILE),']')
         ISTART = LSQBRA + 1
         PREFIX = CHFILE(LSQBRA+1:LSQBRA+2)
#endif
#if defined(CERNLIB_UNIX)
         ISTART = 1
         LSLASH = INDEXB(CHFILE(1:LFILE),'/')
         PREFIX = CHFILE(LSLASH+1:LSLASH+2)
#endif
#if defined(CERNLIB_IBMVM)
         CALL VMCMS('EXEC HDBSERV',IRC)
         IF(IRC.EQ.3.OR.IRC.EQ.99) GOTO 10
         IF(IRC.NE.0) THEN
            WRITE(CHTEXT,9002) IRC
 9002    FORMAT('Return code from EXEC HDBSERV = ',I10)
            CALL CDLOGF(0,CHTEXT)
            GOTO 40
         ENDIF
*
*     Get filename
*
         CALL VMCMS('GLOBALV SELECT *EXEC STACK CDFILE',IC)
         CALL VMRTRM(CHFILE,LFILE)
         PREFIX = CHFILE(1:2)
         ISTART = 1
#endif
*
*     Get name of database file
*
         CALL CLTOU(PREFIX)
*
*     Skip active (ZZ & QQ) files
*
         IF(PREFIX.EQ.'ZZ'.OR.PREFIX.EQ.'QQ') THEN
            NZZ = NZZ + 1
            GOTO 30
         ENDIF
*
*     and last.kumac files...
*
         IF(CHFILE(1:LFILE).EQ.'LAST.KUMAC'.OR.
     +      CHFILE(1:LFILE).EQ.'last.kumac') GOTO 30
 
         IDB = ICNTH(PREFIX,CDPRFX,NFILES)
         IF(IDB.EQ.0) THEN
            WRITE(CHTEXT,9003)
 9003    FORMAT('Cannot find database for following file')
            CALL CDLOGF(0,CHTEXT)
            WRITE(CHTEXT,9004) CHFILE(ISTART:LFILE)
 9004    FORMAT('Filename: ',A)
            CALL CDLOGF(0,CHTEXT)
*
*     Move update file to 'bad' directory
*
            CALL CDBAD(CHFILE,IRC)
            WRITE(CHTEXT,9005) CHFILE(ISTART:LFILE)
 9005 FORMAT('Bad prefix in file ',A)
            CALL CDLOGF(-1,CHTEXT)
            GOTO 40
         ENDIF
#if defined(CERNLIB_IBMVM)
*
*     Strip off <user.address>
*
         ISTART = INDEX(CDFILE(IDB),'>')
         IF(ISTART.EQ.0) ISTART = INDEX(CDFILE(IDB),']')
         ISTART = ISTART + 1
#endif
#if !defined(CERNLIB_IBMVM)
         ISTART = 1
#endif
*
*     Open database file
*
         LRECL = 0
         CHTOP = 'CD' // PREFIX
         CHOPT = 'EP'
         CALL CLTOU(CHTOP)
         CALL CDUPDT(LUNRZ,LUNFZ,CHTOP,CDFILE(IDB)(ISTART:),LRECL,
     +   IDIVD,CHOPT,IRC)
         IF(IRC.NE.0) THEN
            WRITE(CHTEXT,9006) IRC,CHTOP
 9006 FORMAT('Return code ',I3,' from CDUPDT for ',A)
            CALL CDLOGF(-1,CHTEXT)
            GOTO 40
         ENDIF
 
         CALL CDLOGL(CHTOP,ILOGLV,' ',IRC)
*
*     Get unique name for journal file
*
         CALL CDUNIQ(CHJOUR,IRC)
*
*     Open journal file
*
         IF(IDEBCD.GE.1) THEN
            WRITE(CHTEXT,7007) CHTOP,LUNJOU
 7007 FORMAT('Opening journal file ',A,' on unit ',I2)
            CALL CDLOGF(-1,CHTEXT)
         ENDIF
 
         CALL CDJOUR(LUNJOU,CHTOP,CHJOUR,80,'AOZ',IRC)
         IF(IRC.NE.0) THEN
            WRITE(CHTEXT,9007) IRC,CHTOP
 9007 FORMAT('Return code ',I3,' from CDJOUR for ',A)
            CALL CDLOGF(-1,CHTEXT)
            GOTO 40
         ENDIF
*
*     Process update
*
         CALL CDLOAD(LUNFZ,CHFILE,IRC)
         IF(IRC.NE.0) THEN
*
*     Purge new journal file
*
            CLOSE(LUNJOU,STATUS='DELETE')
            GOTO 40
         ENDIF
*
*     Get FZ status information for new journal file
*
         CALL FZINFO(LUNJOU)
 
         IUSED = INFOFZ(2)
*
*     Close new journal file
*
         CALL FZENDO(LUNJOU,'TE')
*
*     Do not send or journal if file unused
*
         IF(IUSED.EQ.0) THEN
            CLOSE(LUNJOU,STATUS='DELETE')
            WRITE(CHTEXT,9008) CHTOP
 9008 FORMAT('Warning - empty output journal file deleted for ',A)
            CALL CDLOGF(-1,CHTEXT)
            GOTO 20
         ENDIF
 
         CLOSE(LUNJOU)
*
*     Send new journal file to list of remote servers
*     This is only for the master server as slaves must,
*     by definition, have received this update from the master
*
         IF(MASTER) CALL CDSEND(CHJOUR,IDB,IRC)
*
*     and save
*
         CALL CDBACK(CHJOUR,IDB,IRC)
         IF(IRC.NE.0) THEN
            WRITE(CHTEXT,9009) IRC,CHTOP
 9009 FORMAT('Return code ',I3,' from CDBACK for ',A)
            CALL CDLOGF(-1,CHTEXT)
            GOTO 40
         ENDIF
 
   20    CONTINUE
*
*     Delete original update file
*
         CALL CDDELF(CHFILE,IRC)
*
*     Close this database
*
         CALL CDEND(CHTOP,'S',IRC)
   30 CONTINUE
*
*     Sleep if we only found ZZ files...
*
      IF(NZZ.EQ.NFOUND) CALL SLEEPF(IWAKE)
 
      GOTO 10
 
   40 CONTINUE
      CALL DATIME(ID,IT)
      WRITE(CHTEXT,9011) 'Stopping at ',ID,IT
 9010 FORMAT(A,I6,1X,I4,1X,A,1X,A)
 9011 FORMAT(A,I6,1X,I4)
      CALL CDLOGF(0,CHTEXT)
 
      END
