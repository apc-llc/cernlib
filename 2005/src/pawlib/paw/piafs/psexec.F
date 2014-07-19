*
* $Id: psexec.F,v 1.7 1998/05/05 08:30:15 couet Exp $
*
* $Log: psexec.F,v $
* Revision 1.7  1998/05/05 08:30:15  couet
* - The last CVS comment produced some errors on AIX
*
* Revision 1.6  1997/08/04 12:29:54  dinofm
* The 'stagealloc' comment has been modified to 'Fortran' syntax 
* to allow compilation on AIX (reported by M. Dahlinger/GSI).
*
* Revision 1.5  1997/02/14 16:18:46  dinofm
* PSSYSTEM function is used to execute SHELL commands on all  platforms.
*
* Revision 1.4  1996/12/04 16:56:21  dinofm
* Take care of the new SHIFT STAGER.
*
* Revision 1.3  1996/09/11 15:01:34  couet
* - Uses hldnt
*
* Revision 1.2  1996/04/24 08:47:37  dinofm
* The PIAF/MESSAGE 'back door' handling allows to retrieve a PQRY message
* (used, e.g., to ask information to the Piaf server).
*
* Revision 1.1.1.1  1996/03/01 11:39:27  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :          31/01/96  11.30.32  by  Timo Hakulinen
*-- Author :    Alfred Nathaniel   28/04/93
      SUBROUTINE PSEXEC(HANGUP)
*
* Receive a client message and execute it
*
#include "hbook/czbuff.inc"
#include "hbook/hcopt.inc"
#include "hbook/hcpiaf.inc"
#include "paw/pawchn.inc"
#include "paw/rzcxio.inc"
#include "paw/quest.inc"
*

* When compiled on SUN we assume is the CS2, thus stagealloc is used 

#if defined(CERNLIB_SUN)
#  define CERNLIB_STAGEALLOC
#endif

      INTEGER PSSYSTEM

      INTEGER HANGUP

      PARAMETER    (MAXDIR = 100)
      CHARACTER*16  CHDIRR(MAXDIR)
      CHARACTER*1   CHTYPE
      LOGICAL       PROPAG

      CHARACTER CHMAIL*80, CHMESS*6, CHLINE*74, CHOPT*14, CHDIR*60
      CHARACTER OBNAME*40, OBCLAS*20, STEXT*20, LTEXT*80, CHPID*12
      CHARACTER CHLCL*80

      SAVE      NF

*
* Listen to client (in case of interrupt this is necessary)
*
      CALL PFSOCK(0)
*
* In case of a SIGHUP signal try to close the master and slave servers in
* a clean way so that PALOGS will be called
*
      IF (HANGUP .NE. 0) THEN
         CHMAIL = '*CLOSE'
         GOTO 2
      ENDIF

      NSLBADO = NSLBAD
*
* Loop over client messages
*
 1    CONTINUE
*
* In case a slave crashes send a new group view to the remaining slaves
*
      IF (MASTPF .AND. NSLBAD.NE.NSLBADO) THEN
         CALL PSMGRP(NSLAVE)
         NSLBADO = NSLBAD
      ENDIF
*
* After 10 hours of inactivity Piaf will shut itself down (an orphaned
* slave will terminate after 11 hours)
*
      IF (MASTPF) THEN
         CALL PSALARM(10)
      ELSE
         CALL PSALARM(11)
      ENDIF

      CALL CZGETA(CHMAIL,ISTAT)
      IF (ISTAT.NE.0) CHMAIL = '*CLOSE'

 2    CALL DEBUG

      CHMESS=CHMAIL(:6)
      CHLINE=CHMAIL(7:)

      CALL PFLOG(8,'PSEXEC',CHMAIL)

      IF (CHMESS(:5).EQ.'KUIP ') THEN
*
* Execute a KUIP command
*
         IF (CHMESS(6:6).EQ.'+') THEN
*--- long command line
            READ(CHLINE,'(I6)') L
            CALL CZGETC(L,ISTAT)
         ELSE
            L=LENOCC(CHLINE)
            CHBUF(:L)=CHLINE
         ENDIF
         CALL PSKUIP(CHBUF(1:L),ISTAT)

      ELSEIF (CHMESS.EQ.'PACUTS') THEN
*
* Define graphical cuts
*
         CALL PSPAW(CHMESS, CHLINE, ISTAT)

      ELSEIF (CHMESS.EQ.'PCHROP') THEN
*
* Open chain member
*
         CALL PSPAW(CHMESS, CHLINE, ISTAT)

      ELSEIF (CHMESS.EQ.'GROUP:') THEN
*
* Get the current slave group view: # of active slaves and own unique id.
* This message will only be send by the master to all active slave servers.
* If the group view changes free all dynamic Ntuple buffers, since the
* range of events to be handled by each slave depends on the size of the
* group.
*
         NGOLD = NGSIZE
         READ(CHLINE,'(I5,I5)') NGSIZE, MYSID
         IF (NGSIZE .NE. NGOLD) CALL HBFREE(0)

      ELSEIF (CHMESS.EQ.'HCDIR:') THEN
*
* Change directory
*
         CHOPT=CHLINE(:14)
         CHDIR=CHLINE(15:)
         IQUEST(1)=0
         IF (INDEX(CHOPT,'R').NE.0) THEN
            L=LEN(CHDIR)
         ELSE
            L=LENOCC(CHDIR)
            IF (L.EQ.0) L=1
         ENDIF
         CALL HCDIR(CHDIR(:L),CHOPT)
         IQ1=IQUEST(1)
*
* If directory change was succesful propagate to slave servers
*
         IF (MASTPF .AND. IQ1.EQ.0) THEN
            CALL PSCAST(CHMAIL, NSLAVE, ISTAT)
            CALL PSLOOP(NSLAVE, ISTAT)
         ENDIF
*
         CALL PSPLOG(IQUEST(1),ISTAT)
         IF (MASTPF .AND. IQ1.EQ.0 .AND. INDEX(CHOPT,'R').NE.0) THEN
            CALL CZPUTA(CHDIR,ISTAT)
         ENDIF

      ELSEIF (CHMESS.EQ.'HLDIR:') THEN
*
* List directory
*
         CHOPT=CHLINE(:14)
         CHDIR=CHLINE(15:)
         L=LENOCC(CHDIR)
         IF (L.EQ.0) L=1
         CALL HLDIR(CHDIR(:L),CHOPT)
         CALL PSPLOG(IQUEST(1),ISTAT)

      ELSEIF (CHMESS .EQ. 'HRDIR:') THEN
*
* Retrieve list of subdirectories
*
         READ(CHLINE, '(I4)') MXDIR
         CALL HRDIR(MAXDIR, CHDIRR, NDIR)
         CALL PSPLOG(IQUEST(1),ISTAT)
         NDIR = MIN(MXDIR, NDIR)
         WRITE(CHBUF, '(I4)') NDIR
         DO 12 I = 1, NDIR
            CHBUF(5+16*(I-1):5+16*I-1) = CHDIRR(I)
  12     CONTINUE
         CALL CZPUTC(5+16*NDIR-1, ISTAT)

      ELSEIF (CHMESS .EQ. 'HLNEXT') THEN
*
* Return element by element the contents of a file
*
         READ(CHLINE(2:),'(I8,A)') IDH, CHOPT
         CALL HLNEXT(IDH, CHTYPE, CHDIR, CHOPT)
         IQUEST(1)  = IDH
         CHMAIL     = CHTYPE
         CHMAIL(2:) = CHDIR
         CALL PSPLOG(IQUEST(1),ISTAT)
         CALL CZPUTA(CHMAIL,ISTAT)

      ELSEIF (CHMESS .EQ. 'HGETNT') THEN
*
* Execute HNTLD in case of a chain and return the number of rows in the chain
*
* Strip-off the variable from the Ntuple ID. In case it is a function and
* it contains vectors the Comis translator tries to get the vector while
* the client does not expect that (dead-lock).
*
         I = INDEX(CHLINE, '.')
         IF (I .GT. 2) THEN
            CHDIR = CHLINE(2:I-1)
         ELSE
            CHDIR = CHLINE(2:)
         ENDIF
         CALL HNTLD(CHDIR)
         IQUEST(1) = NCHEVT
         CALL PSPLOG(IQUEST(1),ISTAT)

      ELSEIF (CHMESS.EQ.'CZIN  ') THEN
*
* Read request from client's HRZIN
*
         CALL PSRZIN(CHLINE)

      ELSEIF (CHMESS.EQ.'HINPF') THEN
*
* Client wants to push a histogram to the server
*
         READ(CHLINE,'(I12)') IDH
         CALL HINPF(IDH,0)
*
* Propagate to active slave servers
*
         IF (MASTPF) THEN
            DO 26 I = 1, NSLAVE
               CALL PFSOCK(I)
               CALL PFHOUT(IDH, ISTAT)
26          CONTINUE
            CALL PFSOCK(0)
         ENDIF

      ELSEIF (CHMESS.EQ.'INQRZ:') THEN
*
* Client wants to know the record length of an RZ file
*
         LRECL=0
         CALL RZOPEN(10,'INQRZ',CHLINE,'X',LRECL,ISTAT)
         IF (ISTAT.EQ.0) THEN
            WRITE(CHMAIL,'(I8)') LRECL*4
            CALL RZEND('INQRZ')
            CLOSE(10)
         ELSE
            WRITE(CHMAIL,'(I8,A)') 0,' Cannot determine RZ record size'
         ENDIF
         CALL CZPUTA(CHMAIL,ISTAT)

      ELSEIF (CHMESS.EQ.'GETA :') THEN
*
* Client wants to receive a file
*
         CALL PSGETA(CLINPF,LOGLPF)
         CALL PSPLOG(IQUEST(1),ISTAT)

      ELSEIF (CHMESS.EQ.'PUTA :') THEN
*
* Client wants to push a file
*
         LF = 0
         IF (CHLINE .NE. ' ') THEN
            READ(CHLINE,'(I12)') LF
         ENDIF
         CALL PSPUTA(CLINPF,LOGLPF,LF)
         CALL PSPLOG(IQUEST(1),ISTAT)

      ELSEIF (CHMESS.EQ.'CSEXEC') THEN
*
* The pushed Fortran file has to be COMIS compiled
*
         CALL PAWCS

         PROPAG = .TRUE.
         LF = LENOCC(CHLINE)
         IF (CHLINE(LF-1:LF) .EQ. '77') PROPAG = .FALSE.
         IF (CHLINE(LF-1:LF) .EQ. '.c') PROPAG = .FALSE.

         IF (MASTPF .AND. .NOT.PROPAG) THEN
*
* Tell slaves and master to unload shared library
*
            CALL HITOC(MASPID, CHPID, LP, IERROR)
            I = INDEXB(CHLINE(:LF), '.')
            WRITE(CHSMPF,'(A)') 'CSRMSL'//CHLINE(:I-1)//'_'//CHPID(1:LP)
            CALL PSCAST(CHSMPF,  NSLVPF, ISTAT)

            CALL CSRMSL(CHSMPF(7:))
            CALL UNLINKF(CHSMPF(7:LENOCC(CHSMPF))//'.sl')
         ENDIF

         CHLMPF = CHLINE
         CALL KUHOME(CHLMPF, LF)
         CALL CSEXEC('!FILE '//CHLMPF,ISTAT)
         IQUEST(1)=ISTAT

*
* If Comis compilation was succesful compile also on slave servers
*
         IF (MASTPF .AND. PROPAG .AND. ISTAT.EQ.0) THEN
            CALL PSCAST(CHMAIL, NSLAVE, ISTAT)
            CALL PSLOOP(NSLAVE, ISTAT)
         ENDIF
*
         CALL PSPLOG(IQUEST(1),ISTAT)

      ELSEIF (CHMESS.EQ.'CSRMSL') THEN
*
* Unload shared library
* This message will only be send by the master to all slave servers
*
         CHLMPF = CHLINE
         CALL KUHOME(CHLMPF, LF)
         CALL CSRMSL(CHLMPF)
         CALL UNLINKF(CHLINE(1:LENOCC(CHLINE))//'.sl')

      ELSEIF (CHMESS.EQ.'SHELL:') THEN
*
* Execute a local shell command
*
*-- avoid security leak, e.g 'ls; rm -r /'
         L=LENOCC(CHLINE)
         IF (INDEX(CHLINE(:L),';').EQ.0 .AND.
     +       INDEX(CHLINE(:L),'|').EQ.0 .AND.
     +       INDEX(CHLINE(:L),'&').EQ.0 .AND.
     +       INDEX(CHLINE(:L),'>').EQ.0 .AND.
     +       INDEX(CHLINE(:L),'<').EQ.0 .AND.
     +       INDEX(CHLINE(:L),'`').EQ.0 .AND.
     +      (CHLINE(:4).EQ.'cat' .OR. CHLINE(:3).EQ.'cp' .OR.
     +       CHLINE(:3).EQ.'ls'  .OR. CHLINE(:3).EQ.'mv' .OR.
     +       CHLINE(:3).EQ.'rm'  .OR. CHLINE(:4).EQ.'pwd' .OR.
     +       CHLINE(:6).EQ.'mkdir' .OR. CHLINE(:6).EQ.'rmdir')) THEN
#if defined(CERNLIB_CORE)||defined(CERNLIB_STAGEALLOC)
*
* Special case for rm and DPM.
*
            IF (CHLINE(:3) .EQ. 'rm') THEN
               CALL PSRM(CHLINE(3:L))
*
* Special case for cp and DPM.
*
            ELSEIF (CHLINE(:3) .EQ. 'cp') THEN
               CALL PSCP(CHLINE(3:L))
*
* Special case for mv and DPM.
*
            ELSEIF (CHLINE(:3) .EQ. 'mv') THEN
               CALL PSMV(CHLINE(3:L))
            ELSE
               IQUEST(1)=PSSYSTEM(CHLINE(:L))
            ENDIF
#else
            IQUEST(1)=PSSYSTEM(CHLINE(:L))
#endif
         ELSE
            PRINT *,' Invalid command: ',CHLINE(:L)
            IQUEST(1)=1
         ENDIF
         CALL PSPLOG(IQUEST(1),ISTAT)

      ELSEIF (CHMESS.EQ.'MESS :') THEN
*
* Back door for new commands
*
         L = LENOCC(CHLINE)
         CALL DECODEMESS (CHLINE,IRET)
         IF (IRET.EQ.0) THEN
            PRINT *,' Invalid message: ',CHLINE(:L)
         ELSE
            PRINT *,CHLINE
         ENDIF
         IQUEST(1) = 0
         CALL PSPLOG(IQUEST(1),ISTAT)

      ELSEIF (CHMESS.EQ.'PING :') THEN
*
* Noop, just to check if server is still alive
*

      ELSEIF (CHMESS.EQ.'HCOPT:') THEN
*
* Set HCOPT words
*
         READ(CHLINE,'(10I5)') (ICOPT(I),I=1,10)
*
* Propagate to slave servers
*
         IF (MASTPF) THEN
            CALL PSCAST(CHMAIL, NSLAVE, ISTAT)
         ENDIF

      ELSEIF (CHMESS.EQ.'LOGLEV') THEN
*
* Set log level
*
         READ(CHLINE,'(I12)') LOGLPF
*
* Propagate log level to slave servers
*
         IF (MASTPF) THEN
            CALL PSCAST(CHMAIL, NSLAVE, ISTAT)
         ENDIF

      ELSEIF (CHMESS.EQ.'STATUS') THEN
*
* Report current server status, including IO and CPU statistics of slaves
*
         CALL PSLVIO(NSLVPF, ISTAT)
         TOTIO = RZXIN + RZXOUT + SRIN + SROUT
         CALL PSSTAT(NSLAVE,TOTIO,SVMIO,STUSER,STCPU)
         CALL PSPLOG(IQUEST(1),ISTAT)

      ELSEIF (CHMESS.EQ.'IOSTAT') THEN
*
* Collect the I/O (real and virtual) and CPU statistics from all slaves
* This message will only be send by the master to all slave servers
*
            CALL HBVM(VIO)
            CALL PSCPU(TUSER, TCPU)
            WRITE(CHSMPF,'(3F16.0,2F16.3)') RZXIN, RZXOUT, VIO, TUSER,
     +                                      TCPU
            CALL CZPUTA(CHSMPF, ISTAT)

      ELSEIF (CHMESS.EQ.'GETCWD') THEN
*
* Return current working directory
*
         CALL GETWDF(CHMAIL)
         DO 10 I = LENOCC(CHMAIL), 1, -1
            IF (CHMAIL(I:I) .EQ. '/') THEN
               CALL CZPUTA('~'//CHMAIL(I:),ISTAT)
               GOTO 1
            ENDIF
  10     CONTINUE
         CALL CZPUTA(CHMAIL,ISTAT)

      ELSEIF (CHMESS.EQ.'SCAND:') THEN
*
* Return one by one the files in the Piaf directory
*
         IF (CHLINE .NE. ' ') THEN
            NF   = 0
            LP   = LENOCC(CHLINE)
            CALL PSSCAN(CHLINE, LP, NF, OBNAME, OBCLAS, STEXT, LTEXT)
         ELSE
            NF = NF + 1
            CALL PSSCAN(' ', 0, NF, OBNAME, OBCLAS, STEXT, LTEXT)
         ENDIF
         CALL CZPUTA(OBNAME//OBCLAS//STEXT, ISTAT)
         IF (OBNAME .NE. ' ') THEN
            CALL CZPUTA(LTEXT, ISTAT)
         ENDIF

      ELSEIF (CHMESS.EQ.'MODE :') THEN
*
* Switch server mode
*
         IF (CHLINE(1:1).EQ.'?') THEN
            IF (NSLAVE .EQ. 0) THEN
               PRINT *,' Piaf server running in sequential mode'
            ELSE
               PRINT 10000, NSLAVE
            ENDIF
         ELSEIF (CHLINE(1:3).EQ.'SEQ') THEN
            NSLAVE = 0
            PRINT *,' Piaf server set to sequential mode'
         ELSEIF (CHLINE(1:3).EQ.'PAR') THEN
            READ(CHLINE(4:),'(I12)') NSLAV
            CALL PSPAR(NSLAV, 1)
         ELSE
            PRINT *,' Invalid Piaf server mode ', CHLINE(1:3)
         ENDIF
*
* Tell client that mode change has finished
*
         CALL CZPUTA('MODE*',ISTAT)
         CALL PSPLOG(IQUEST(1),ISTAT)

      ELSEIF (CHMESS.EQ.'STAGE:') THEN
*
* Submit a staging request
*
         CALL CZGETA(CHMAIL,ISTAT)
         IF (INDEX(CHLINE(:8),'L').NE.0) THEN
            CALL CZGETA(CHLCL,ISTAT)
         ELSE
            CHLCL=' '
         ENDIF
         CALL PSTAGE(CHMAIL,CHLINE(9:),CHLINE(:8),CHLCL)
         CALL PSPLOG(IQUEST(1),ISTAT)

      ELSEIF (CHMESS.EQ.'*CLOSE') THEN
*
* Close all slaves
*
         IF (MASTPF) THEN
            CALL PSLVIO(NSLVPF, ISTAT)
            CALL PSCAST(CHMESS, NSLVPF, ISTAT)
         ENDIF
*
* Close connection
*
         GOTO 9

      ELSE
*
* Unknown message: close connection
*
         L=LENOCC(CHMAIL)
         IF (L.EQ.0) L=1
         PRINT '(2A)','  Unknown message: ',CHMAIL(:L)
         CHMAIL = '*CLOSE'
         GOTO 2
      ENDIF
*
      GOTO 1
*
 9    CONTINUE
*
10000 FORMAT('  Piaf server running in parallel mode (',I1,' slaves)')
*
      END
