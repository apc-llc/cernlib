*
* $Id: cdmove.F,v 1.4 1996/04/12 09:42:25 cernlib Exp $
*
* $Log: cdmove.F,v $
* Revision 1.4  1996/04/12 09:42:25  cernlib
* New handling of title
*
* Revision 1.3  1996/03/29 11:18:20  jamie
* qftitlch
*
* Revision 1.2  1996/03/12 13:07:39  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:34  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CDMOVE
*******************************************************************************
*                                                                             *
*             ####   #####   #    #   ####   #    #  ######                   *
*            #    #  #    #  ##  ##  #    #  #    #  #                        *
*            #       #    #  # ## #  #    #  #    #  #####                    *
*            #       #    #  #    #  #    #  #    #  #                        *
*            #    #  #    #  #    #  #    #   #  #   #                        *
*             ####   #####   #    #   ####     ##    ######                   *
*                                                                             *
*******************************************************************************
*                                                                             *
*     Send HEPDB journal files to slave servers                               *
*     This version is for VM, Unix and VAX/VMS systems only                   *
*     (MVS version requires XZLLS, GETENVF)                                   *
*     Can be used to transfer updates to MAXSRV remote machines               *
*     Updates can be for different groups.                                    *
*                                                                             *
*******************************************************************************
 
      PARAMETER     (MAXFIL=1000)
      PARAMETER     (MAXSRV=50)
      PARAMETER     (MAXGRP=50)
      PARAMETER     (IPRINT=6)
      PARAMETER     (LUNI=1)
      PARAMETER     (LUNO=2)
      PARAMETER     (LUNN=3)
      COMMON/PAWC/PAW(50000)
#include "hepdb/cduscm.inc"
#include "hepdb/slate.inc"
#include "zebra/quest.inc"
      COMMON/CZSOCK/LUNCZ,IADTCP,LBUF,ISKIN,ISKOUT,IPROT
      CHARACTER*255 FILES(MAXFIL),CHFILE,CHPATT,CHDIR
      CHARACTER*20  CHIN(2,10)
      CHARACTER*255 CHOUT(2,20)
      CHARACTER*255 CHSERV,CHLIST,CHSTOP
      CHARACTER*255 CHPATH(MAXSRV)
      CHARACTER*255 GENAM,REMDIR,REMFIL,LOCFIL,TODO
      CHARACTER*20  CHTEMP
      CHARACTER*80  CHMAIL
      CHARACTER*8   CHWAKE,CHLOGL,USER,CHHOST,CHTYPE,CHSYS
      CHARACTER*8   GROUP
      CHARACTER*8   CHGRP (MAXGRP)
      CHARACTER*20  CHNAME(MAXSRV)
      CHARACTER*20  CHUSER(MAXSRV)
* Increase to 80 for node names like hpikf1_f.ikf.physik.uni-frankfurt.de
      CHARACTER*80  CHNODE(MAXSRV)
      CHARACTER*20  CHPROT(MAXSRV)
      CHARACTER*20  CHRECV(MAXSRV)
      CHARACTER*20  CHPOLL(MAXSRV)
      CHARACTER*80  CHQUED(MAXSRV)
      CHARACTER*80  CHLQUE(MAXSRV)
      CHARACTER*80  CHRQUE(MAXSRV)
      CHARACTER*2   PREFIX
      CHARACTER*1   CHMODE,CHOPT
      DIMENSION     LGRP (MAXGRP)
      DIMENSION     LPATH(MAXSRV),LNAME(MAXSRV),LUSER(MAXSRV),
     +              LNODE(MAXSRV),LPROT(MAXSRV),LPOLL(MAXSRV),
     +              LRECV(MAXSRV),LQUED(MAXSRV),LLQUE(MAXSRV),
     +              LRQUE(MAXSRV)
      LOGICAL       IEXIST,IOPEN
#include "hepdb/qftitlch.inc"
      CHARACTER    VIDQQ*(*)
      PARAMETER   (VIDQQ = '@(#)' //
     + HepdbTitleFortranString
     + // '>')
      CALL CLEFT (VIDQQ,1,0)
 
      LPRTCD = IPRINT
#if defined(CERNLIB_VAXVMS)
      OPEN(LPRTCD,FILE='SYS$OUTPUT',RECL=512,STATUS='UNKNOWN',
     +     FORM='FORMATTED',ACCESS='SEQUENTIAL')
#endif
*
*     Initialise ZEBRA
*
      CALL HLIMIT(50000)
*
*     Print version number
*
      CALL CDVERS(' ',' ','P')
*
*     Get host name
*
      CALL CDHOST(CHHOST,IRC)
      LHOST = LENOCC(CHHOST)
      CALL CLTOU(CHHOST(1:LHOST))
*
*     Get list of users to process
*
      CALL GETENVF('CDGROUPS',CHLIST)
      LLIST = IS(1)
      IF(LLIST.EQ.0) THEN
         NUSERS = 1
*
*     Get our name
*
         CALL GETENVF('CDGRP',CHGRP(1))
         LGRP(1) = IS(1)
         IF(IS(1).EQ.0) THEN
            CALL CDUSER(CHGRP(1),IRC)
            LGRP(1) = LENOCC(CHGRP(1))
         ENDIF
         CALL CLTOU(CHGRP(1)(1:LGRP(1)))
      ELSE
         CALL CLTOU(CHLIST(1:LLIST))
         CALL CDNWRD(',',CHLIST(1:LLIST),NUSERS)
         DO 10  I=1,NUSERS
            CALL CDWORD(CHGRP(I),I-1,',',CHLIST(1:LLIST),IRC)
            LGRP(I) = LENOCC(CHGRP(I))
   10    CONTINUE
      ENDIF
*
*     Get stop file
*
      CALL GETENVF('CDSTOP',CHSTOP)
      LSTOP = IS(1)
*
*     Initialise counters
*
      NSENT = 0
      NGOT  = 0
*
*     Get the log level
*
      CALL GETENVF('CDLOGL',CHLOGL)
      IF(IS(1).GT.0) THEN
         IDEBCD = ICDECI(CHLOGL,1,8)
      ELSE
         IDEBCD = 0
      ENDIF
 
      IF(IDEBCD.GT.0) THEN
         CHOPT = 'S'
      ELSE
         CHOPT = ' '
      ENDIF
*
*     Get the wakeup interval
*
      CALL GETENVF('CDWAKEUP',CHWAKE)
      IF(IS(1).GT.0) THEN
         ISLEEP = ICDECI(CHWAKE,1,8)
      ELSE
         ISLEEP = 60
      ENDIF
*
*     Initialise XZ
*
      CALL XZINIT(IPRINT,IDEBCD,LUNI,LUNO)
 
   20 CONTINUE
 
      DO 130 N=1,NUSERS
 
         IF(IDEBCD.GE.0) WRITE(LPRTCD,9001) CHGRP(N)(1:LGRP(N))
 9001 FORMAT(/,' CDMOVE. processing group ',A)
 
         IF(CHGRP(N)(1:2).EQ.'CD') THEN
            GROUP  = CHGRP(N)(1:LGRP(N))
            LGROUP = LGRP(N)
         ELSE
            GROUP  = 'CD'//CHGRP(N)(1:LGRP(N))
            LGROUP = LGRP(N) + 2
         ENDIF
*
*     Get location of names file
*
         CALL GETENVF(GROUP(1:LGROUP),CHDIR)
         LDIR = IS(1)
         IF(LDIR.EQ.0) THEN
            CHFILE = 'HEPDB.NAMES'
         ELSE
#if defined(CERNLIB_IBMMVS)
            CHFILE = CHDIR(1:LDIR)//'.'//
     +         'HEPDB.NAMES'
#endif
#if defined(CERNLIB_VAXVMS)
            CHFILE = CHDIR(1:LDIR)//
     +         'HEPDB.NAMES'
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_WINNT))&&(!defined(CERNLIB_MSDOS))
            CHFILE = CHDIR(1:LDIR)//'/'//
     +         'HEPDB.NAMES'
#endif
#if defined(CERNLIB_WINNT)
            CHFILE = CHDIR(1:LDIR)//'\\'//
     +         'HEPDB.NAMES'
#endif
#if defined(CERNLIB_MSDOS)
            CHFILE = CHDIR(1:LDIR)//'\\'//
     +         'HEPDB.NAM'
#endif
         ENDIF
         LF = LENOCC(CHFILE)
#if defined(CERNLIB_UNIX)
         CALL CUTOL(CHFILE(1:LF))
#endif
#if defined(CERNLIB_IBMMVS)
*
*     Override with names files entries
*
         CHIN(1,1)  = ':nick'
         CHIN(2,1)  = 'CDMOVE'
 
         CHOUT(1,1) = ':wakeup'
         CHOUT(2,1) = ' '
         CHOUT(1,2) = ':logl'
         CHOUT(2,2) = ' '
 
         NIN        = 1
         NOUT       = 2
         CALL NAMEFD(LUNN,CHFILE(1:LF),CHIN,NIN,CHOUT,NOUT,IRC)
         IF(IRC.EQ.0) THEN
            LOUT = LENOCC(CHOUT(2,1))
            IF(LOUT.NE.0) ISLEEP = ICDECI(CHOUT(2,1),1,LOUT)
            LOUT = LENOCC(CHOUT(2,2))
            IF(LOUT.NE.0) IDEBCD = ICDECI(CHOUT(2,1),1,LOUT)
         ENDIF
#endif
*
*     Get the list of databases for this group
*
         CHIN(1,1)  = ':nick'
         CHIN(2,1)  = 'CONFIG'
 
         CHOUT(1,1) = ':servers'
 
         NIN        = 1
         NOUT       = 1
         LUN        = 1
 
         IF(IDEBCD.GE.1) WRITE(LPRTCD,9002) CHFILE(1:LF)
 9002 FORMAT(' CDMOVE. processing names file ',A)
         CALL NAMEFD(LUNN,CHFILE(1:LF),CHIN,NIN,CHOUT,NOUT,IRC)
         IF(IRC.NE.0) THEN
            WRITE(LPRTCD,9003) IRC
 9003 FORMAT(' CDMOVE. error from NAMEFD, IRC = ',I10)
            GOTO 140
         ENDIF
*
*     For each server, get
*        :node
*        :userid
*
*     :userid gives us the name of the HEPDB group for whom
*     we are working. e.g.
*     :userid.cdcplear
*     We expect an environmental variable/symbol of this
*     name (uppercase), e.g.
*     CDCPLEAR:==DISK$MF:[CDCPLEAR]
*     This has subdirectories [.TOnode], e.g.
*     :node.CPUX04 = [.TOCPUX04] unless overridden by tag :localq
*
         CHSERV = CHOUT(2,1)
         CALL CSQMBL(CHSERV,1,LEN(CHSERV))
         LCHSERV = LENOCC(CHSERV)
 
         IF(LCHSERV.EQ.0) THEN
            WRITE(LPRTCD,9004) GROUP(1:LGROUP)
 9004 FORMAT(' CDMOVE. no servers defined for ',A)
            WRITE(LPRTCD,9005)
 9005 FORMAT(' CDMOVE. The :servers tag for the :nick.config entry',
     +       ' must contain a list of all remote servers.')
            IF(NUSERS.EQ.1) THEN
               GOTO 140
            ELSE
               GOTO 130
            ENDIF
         ENDIF
 
         CALL CDNWRD(' ',CHSERV(1:LCHSERV),NSERV)
 
         IF(NSERV.GT.MAXSRV) THEN
            WRITE(LPRTCD,9006) MAXSRV
 9006 FORMAT(' CDMOVE. cannot process more than ',I6,' servers')
            NSERV = MAXSRV
         ENDIF
 
         DO 30 I=1,NSERV
            CALL CDWORD(CHNAME(I),I-1,' ',CHSERV(1:LCHSERV),IRC)
            IF(IDEBCD.GE.1) WRITE(LPRTCD,9007) CHNAME(I)
 9007 FORMAT(/,' CDMOVE. processing node ',A)
            CHIN(1,1) = ':nick'
            CHIN(2,1) = CHNAME(I)
 
            CHOUT(1,1) = ':userid'
            CHOUT(2,1) = ' '
            CHOUT(1,2) = ':node'
            CHOUT(2,2) = ' '
            CHOUT(1,3) = ':protocol'
            CHOUT(2,3) = ' '
            CHOUT(1,4) = ':receive'
            CHOUT(2,4) = ' '
            CHOUT(1,5) = ':queue'
            CHOUT(2,5) = ' '
            CHOUT(1,6) = ':localq'
            CHOUT(2,6) = ' '
            CHOUT(1,7) = ':remoteq'
            CHOUT(2,7) = ' '
            CHOUT(1,8) = ':poll'
            CHOUT(2,8) = ' '
#if defined(CERNLIB_IBMMVS)
            CHOUT(1,9) = ':mvsid'
            CHOUT(2,9) = ' '
#endif
 
            NIN = 1
#if defined(CERNLIB_IBMMVS)
            NOUT = 9
#endif
#if !defined(CERNLIB_IBMMVS)
            NOUT = 8
#endif
 
            CALL NAMEFD(LUNN,CHFILE(1:LF),CHIN,NIN,CHOUT,NOUT,IRC)
 
            LUSER(I) = LENOCC(CHOUT(2,1))
            CHUSER(I) = CHOUT(2,1)(1:LUSER(I))
 
            LNODE(I) = LENOCC(CHOUT(2,2))
            CHNODE(I) = CHOUT(2,2)(1:LNODE(I))
 
            LPROT(I) = LENOCC(CHOUT(2,3))
            IF(LPROT(I).EQ.0) THEN
               LPROT(I)  = 18
               CHPROT(I) = 'TCP/IP (defaulted)'
            ELSE
               CHPROT(I) = CHOUT(2,3)(1:LPROT(I))
               CALL CLTOU(CHPROT(I)(1:LPROT(I)))
            ENDIF
 
            LRECV(I) = LENOCC(CHOUT(2,4))
            IF(LRECV(I).EQ.0) THEN
               LRECV(I)  = 14
               CHRECV(I) = 'NO (defaulted)'
            ELSE
               CHRECV(I) = CHOUT(2,4)(1:LRECV(I))
               CALL CLTOU(CHRECV(I)(1:LRECV(I)))
            ENDIF
 
            LQUED(I) = LENOCC(CHOUT(2,5))
            CHQUED(I) = CHOUT(2,5)(1:LQUED(I))
 
            LLQUE(I) = LENOCC(CHOUT(2,6))
            CHLQUE(I) = CHOUT(2,6)(1:LLQUE(I))
#if defined(CERNLIB_IBMVM)
            IF(LLQUE(I).EQ.0) THEN
               CHLQUE(I) = 'B'
               LLQUE(I)  = 1
               WRITE(LPRTCD,9008)
 9008 FORMAT(' CDMOVE. local queue defaulted to mode B')
            ENDIF
#endif
 
            LRQUE(I) = LENOCC(CHOUT(2,7))
            CHRQUE(I) = CHOUT(2,7)(1:LRQUE(I))
 
            LPOLL(I) = LENOCC(CHOUT(2,8))
            IF(LPOLL(I).EQ.0) THEN
               LPOLL(I)  = 14
               CHPOLL(I) = 'NO (defaulted)'
            ELSE
               CHPOLL(I) = CHOUT(2,8)(1:LPOLL(I))
               CALL CLTOU(CHPOLL(I)(1:LPOLL(I)))
            ENDIF
 
#if defined(CERNLIB_IBMMVS)
            LPATH(I) = LENOCC(CHOUT(2,9))
            CHPATH(I) = CHOUT(2,9)(1:LPATH(I))
            CALL CLTOU(CHPATH(I)(1:LPATH(I)))
#endif
 
            IF(IDEBCD.GE.0) THEN
               WRITE(LPRTCD,*)
               WRITE(LPRTCD,9009) CHUSER(I)(1:LUSER(I))
 9009 FORMAT(' CDMOVE. remote user  : ',A)
               WRITE(LPRTCD,9010) CHNODE(I)(1:LNODE(I))
 9010 FORMAT(' CDMOVE. remote node  : ',A)
               WRITE(LPRTCD,9011) CHPROT(I)(1:LPROT(I))
 9011 FORMAT(' CDMOVE. protocol     : ',A)
               WRITE(LPRTCD,9012) CHRECV(I)(1:LRECV(I))
 9012 FORMAT(' CDMOVE. receive?     : ',A)
 
               IF(LQUED(I).EQ.0) THEN
                  WRITE(LPRTCD,9013) '(TODO subdirectory)'
               ELSE
                  WRITE(LPRTCD,9013) CHQUED(I)(1:LQUED(I))
 9013 FORMAT(' CDMOVE. server queue : ',A)
               ENDIF
 
               WRITE(LPRTCD,9014) CHLQUE(I)(1:LLQUE(I))
 9014 FORMAT(' CDMOVE. local  queue : ',A)
 
               IF(LRQUE(I).EQ.0) THEN
                  WRITE(LPRTCD,9015) '(QUEUE subdirectory)'
               ELSE
                  WRITE(LPRTCD,9015) CHRQUE(I)(1:LRQUE(I))
 9015 FORMAT(' CDMOVE. remote queue : ',A)
               ENDIF
 
               WRITE(LPRTCD,9016) CHPOLL(I)(1:LPOLL(I))
 9016 FORMAT(' CDMOVE. poll remote? : ',A)
            ENDIF
 
            IF(CHRECV(I)(1:LRECV(I)).EQ.'YES') THEN
               LRECV(I) = 0
            ELSE
               IF(INDEX(CHPROT(I)(1:LPROT(I)),'BITNET').NE.0) THEN
                  IF(IDEBCD.GE.-3) WRITE(LPRTCD,9017)
 9017 FORMAT(' CDMOVE. receive option invalid for Bitnet nodes')
                  LRECV(I) = 0
               ENDIF
            ENDIF
 
            IF(CHPOLL(I)(1:LPOLL(I)).NE.'YES') LPOLL(I) = 0
 
            IF(LPOLL(I).NE.0.AND.LRECV(I).NE.0) THEN
               IF(IDEBCD.GT.-3) WRITE(LPRTCD,9018)
 9018  FORMAT(' CDMOVE. warning - POLL option implies RECEIVE')
               LRECV(I) = 0
            ENDIF
 
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_SFS))&&(!defined(CERNLIB_IBMMVS))
*
*     Get the pathname on the local machine where files are located
*
            CHPATH(I) = ' '
            CHTEMP = CHUSER(I)(1:LUSER(I))
            CALL CLTOU(CHTEMP)
            CALL GETENVF(CHTEMP,CHPATH(I))
            LPATH(I) = IS(1)
#endif
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_SFS))
            IF(LPATH(I).EQ.0) THEN
               WRITE(LPRTCD,9019) CHUSER(I)(1:LUSER(I))
 9019 FORMAT(' CDMOVE. error - variable ',A,' is not defined.',
     +       ' Updates will not be processed for this server.')
               CHPATH(I) = GROUP
               LPATH(I)  = LGROUP
            ELSE
               IF(IDEBCD.GE.0) WRITE(LPRTCD,9020) CHPATH(I)(1:LPATH(I))
 9020 FORMAT(' CDMOVE. server dir.  : ',A)
            ENDIF
#endif
 
   30    CONTINUE
*
*     Now we are ready to go.
*
   40    CONTINUE
         DO 120 I=1,NSERV
*
*     Loop over all remote servers
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_SFS))
*
*     Skip if local path undefined
*
            IF(LPATH(I).EQ.0) GOTO 120
#endif
*
*     Look for files in local queue
*
            ICONT = 0
            IF(LLQUE(I).EQ.0) THEN
#if defined(CERNLIB_VAXVMS)
               CHPATT = CHPATH(I)(1:LPATH(I)-1) // '.TO' // CHNODE(I)
     +         (1:LNODE(I)) // CHPATH(I)(LPATH(I):LPATH(I))
               LPATT = LENOCC(CHPATT)
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_WINNT))
               CHPATT = CHPATH(I)(1:LPATH(I)) // '/TO' // CHNODE(I)(1:
     +         LNODE(I))
               LPATT = LENOCC(CHPATT)
               CALL CUTOL(CHPATT(1:LPATT))
#endif
#if defined(CERNLIB_WINNT)
               CHPATT = CHPATH(I)(1:LPATH(I)) // '\\TO' // CHNODE(I)(1:
     +         LNODE(I))
               LPATT = LENOCC(CHPATT)
               CALL CUTOL(CHPATT(1:LPATT))
#endif
#if defined(CERNLIB_IBMMVS)
               CHPATT = CHPATH(I)(1:LPATH(I)) // '.TO' // CHNODE(I)(1:
     +         LNODE(I))
               LPATT = LENOCC(CHPATT)
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_SFS))
*
*     Look for files on the CDLQUE disk
*
               CHPATT = CHUSER(I)(1:LUSER(I)) // ' ' // CHNODE(I)(1:
     +         LNODE(I)) // ' ' //CDLQUE(I)(1:LLQUE(I))
               LPATT = LENOCC(CHPATT)
#endif
            ELSE
               CHPATT = CHLQUE(I)(1:LLQUE(I))
               LPATT  = LLQUE(I)
            ENDIF
#if !defined(CERNLIB_IBMVM)
*
*     Look for signal.stop file
*
            IF(LSTOP.EQ.0) THEN
 
#endif
#if defined(CERNLIB_IBMMVS)
               CHSTOP ='/'//CHPATT(1:LPATT)//'.SIGNAL.STOP'
               LSTOP = LPATT + 13
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_WINNT))&&(!defined(CERNLIB_MSDOS))
               CHSTOP = CHPATT(1:LPATT)//'/signal.stop'
               LSTOP = LPATT + 12
#endif
#if defined(CERNLIB_WINNT)
               CHSTOP = CHPATT(1:LPATT)//'\\signal.stop'
               LSTOP = LPATT + 12
#endif
#if defined(CERNLIB_MSDOS)
               CHSTOP = CHPATT(1:LPATT)//'\\signal.sto'
               LSTOP = LPATT + 11
#endif
#if defined(CERNLIB_VAXVMS)
               CHSTOP = CHPATT(1:LPATT)//'SIGNAL.STOP'
               LSTOP = LPATT + 11
#endif
#if !defined(CERNLIB_IBMVM)
            ENDIF
            INQUIRE(FILE=CHSTOP(1:LSTOP),EXIST=IEXIST)
            IF(IEXIST) THEN
               PRINT *,'CDMOVE. signal.stop file found - stopping'
               GOTO 140
            ENDIF
#endif
 
            ICONT = 0
            IOPEN = .FALSE.
   50       CONTINUE
            CALL XZLLS(CHPATT(1:LPATT),FILES,MAXFIL,NFOUND,ICONT,' ',
     +      IRC)
            IF(IDEBCD.GE.1)
     +      PRINT *,'CDMOVE. ',NFOUND,' files in ',CHPATT(1:LPATT)
            IF(NFOUND.EQ.0.AND.LPOLL(I).EQ.0) GOTO 120
*
*     XZLLS sets IRC to -1 if NFOUND>MAXFIL...
*
            IRC = 0
            NFOUND = MIN(NFOUND,MAXFIL)
*
*     If we are continuing with an existing node, skip the CZOPEN etc.
*
            IF(ICONT.NE.0.AND.IOPEN) GOTO 60
*
            IF(INDEX(CHPROT(I),'BITNET').NE.0) THEN
               IPROT = -1
            ELSEIF(INDEX(CHPROT(I),'MVSJOB').NE.0) THEN
               IPROT = -2
            ELSEIF(INDEX(CHPROT(I),'DECNET').NE.0) THEN
               IPROT = 1
            ELSE
               IPROT = 0
            ENDIF
*
*     Skip CZOPEN for Bitnet  & MVS nodes
*
            IF(IPROT.LT.0) GOTO 60
*
*     Open connection to remote node using specified protocol
*     This assumes that we have a correctly configured .netrc
*     file, or the correct PROXY settings for DECnet connections.
*
            IF(IDEBCD.GE.2) WRITE(LPRTCD,9021) CHNODE(I)(1:LNODE(I))
 9021 FORMAT(' CDMOVE. connecting to node ',A)
            CALL CZOPEN('zserv',CHNODE(I)(1:LNODE(I)),IRC)
            IF(IRC.NE.0) THEN
               PRINT *,'CDMOVE. cannot connect to ', CHNODE(I)(1:
     +         LNODE(I))
               GOTO 120
            ENDIF
*
*     Get current directory - this will tell us what sort
*     of machine we are talking to...
*
            CALL XZPWD(REMDIR,IRC)
            LRDIR = LENOCC(REMDIR)
*
*     Get type of machine
*
            IVAX = 0
            IF((INDEX(REMDIR(1:LRDIR),']').NE.0).OR. (INDEX(REMDIR(1:
     +      LRDIR),'>').NE.0)) IVAX = 1
 
            IF(IDEBCD.GE.2) WRITE(LPRTCD,9022) REMDIR(1:LRDIR)
 9022 FORMAT(' CDMOVE. remote directory is ',A)
*
*     If remote queue is not defined, assume that we are
*     in home directory of remote server
*
            IF(LQUED(I).EQ.0) THEN
               IF(IVAX.EQ.1) THEN
                  IF(IDEBCD.GE.1) WRITE(LPRTCD,9023) '[.TODO]'
                  CALL XZCD('[.TODO]',IRC)
               ELSE
                  IF(IDEBCD.GE.1) WRITE(LPRTCD,9023) 'TODO'
                  CALL XZCD('TODO',IRC)
               ENDIF
            ELSE
               IF(IDEBCD.GE.1) WRITE(LPRTCD,9023) CHQUED(I)(1:LQUED(I))
               CALL XZCD(CHQUED(I)(1:LQUED(I)),IRC)
            ENDIF
 9023 FORMAT(' CDMOVE. setting remote directory to ',A)
 
            IF(IRC.NE.0) THEN
               WRITE(LPRTCD,9024) IRC
 9024 FORMAT(' CDMOVE. error ',I10,' setting remote directory')
               GOTO 140
            ENDIF
 
   60       CONTINUE
            IOPEN = .TRUE.
 
            DO 70 J=1,NFOUND
               IBRA   = INDEXB(FILES(J),']')
               PREFIX = FILES(J)(IBRA+1:IBRA+2)
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_WINNT))
               CHFILE = CHPATT(1:LPATT)//'/'//FILES(J)
#endif
#if defined(CERNLIB_WINNT)
               CHFILE = CHPATT(1:LPATT)//'\\'//FILES(J)
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_IBMMVS)
               CHFILE = FILES(J)
#endif
*
*     Skip active files
*
               CALL CLTOU(PREFIX)
               IF(PREFIX.EQ.'ZZ') GOTO 70
 
               LF = LENOCC(CHFILE)
*
*     Transfer the file
*
               IF(IPROT.LT.0) THEN
                  CALL FABNET(CHFILE(1:LF),CHUSER(I)(1:LUSER(I)),
     +            CHNODE(I )(1:LNODE(I)),IPROT,IRC)
               ELSE
                  CALL CDUNIQ(REMFIL,IRC)
                  LREM = LENOCC(REMFIL)
                  IF(IDEBCD.GE.1) WRITE(LPRTCD,9025) CHFILE(1:LF),
     +                                       REMFIL(1:LREM)
 9025 FORMAT(' CDMOVE. sending ',A,' as ',A)
                  CALL XZPUTA(CHFILE(1:LF),REMFIL(1:LREM),CHOPT,IRC)
                  IF(IRC.EQ.0) THEN
                     NSENT = NSENT + 1
*
*     Now rename the remote file
*
                     IF(IDEBCD.GE.1) WRITE(LPRTCD,9026) REMFIL(1:LREM),
     +                                          PREFIX//REMFIL(3:LREM)
 9026 FORMAT(' CDMOVE. renaming ',A,' to ',A)
                     CALL XZMV(REMFIL(1:LREM),PREFIX//REMFIL(3:LREM),' '
     +               ,IRC)
                  ENDIF
               ENDIF
               IF(IRC.NE.0) THEN
                  WRITE(LPRTCD,9027) CHFILE(1:LF),CHNODE(I)(1:LNODE(I))
 9027 FORMAT(' CDMOVE. error transferring ',A,' to ',A)
                  ICONT = 0
                  GOTO 110
               ENDIF
*
*     and delete the original if successful
*
               IF(IDEBCD.GE.1) WRITE(LPRTCD,9028) CHFILE(1:LF)
 9028 FORMAT(' CDMOVE. deleting ',A)
               CALL XZLRM(CHFILE(1:LF),IRC)
               IF(IRC.NE.0) WRITE(LPRTCD,9029) IRC,CHFILE(1:LF)
 9029 FORMAT(' CDMOVE. error ',I10,' deleting ',A)
*
*     any remaining files to process?
*
   70       CONTINUE
            IF(ICONT.NE.0) GOTO 50
*
*     Should we receive any files for us?
*
            IF(LRECV(I).EQ.0) THEN
               IF(IDEBCD.GE.1) WRITE(LPRTCD,9023) CHRQUE(I)(1:LRQUE(I))
               IF(LRQUE(I).GT.0) THEN
                  IF(IDEBCD.GE.1)
     +               WRITE(LPRTCD,9023) CHRQUE(I)(1:LRQUE(I))
                  CALL XZCD(CHRQUE(I)(1:LRQUE(I)),IRC)
               ELSE
                  IF(IVAX.EQ.0) THEN
                     IF(IDEBCD.GE.1) WRITE(LPRTCD,9023) '../queue'
                     CALL XZCD('../queue',IRC)
                  ELSE
                     IF(IDEBCD.GE.1) WRITE(LPRTCD,9023) '[-.QUEUE]'
                     CALL XZCD('[-.QUEUE]',IRC)
                  ENDIF
               ENDIF
               IF(IDEBCD.GE.2) THEN
                  CALL XZPWD(REMDIR,IRC)
                  LRDIR = LENOCC(REMDIR)
                  WRITE(LPRTCD,9030) REMDIR(1:LRDIR)
 9030 FORMAT(' CDMOVE. remote directory is ',A)
               ENDIF
*
*        Anything there?
*
               ICONT = 0
   80          CONTINUE
               CHPATT = ' '
               CALL XZLS(CHPATT,FILES,MAXFIL,NFOUND,ICONT,' ',IRC)
               IF(IRC.NE.0.AND.ICONT.EQ.0) THEN
                  IF(IDEBCD.GT.-3) WRITE(LPRTCD,9031) IRC
 9031 FORMAT(' CDMOVE. return code ',I6,' from XZLS')
                  NFOUND = 0
               ENDIF
 
               IF(ICONT.NE.0) THEN
                  PRINT *,'CDMOVE. too many files - excess names '//
     +            'will be flushed'
*
   90             CONTINUE
                  CALL CZGETA(CHMAIL,ISTAT)
                  LCH = LENOCC(CHMAIL)
                  IF(CHMAIL(1:1).EQ.'0') THEN
                  ELSEIF(CHMAIL(1:1).EQ.'1') THEN
                  ELSEIF(CHMAIL(1:1).EQ.'2') THEN
                     GOTO 90
                  ELSEIF(CHMAIL(1:1).EQ.'3') THEN
                     IQUEST(1) = 1
                     IRC = 1
                  ELSEIF(CHMAIL(1:1).EQ.'E') THEN
                     IQUEST(1) = -1
                     IRC = -1
                  ELSEIF(CHMAIL(1:1).EQ.'V') THEN
                     GOTO 90
                  ELSE
                     IQUEST(1) = 1
                     IRC = 1
                  ENDIF
*
               ENDIF
 
               NFOUND = MIN(NFOUND,MAXFIL)
 
               DO 100 J=1,NFOUND
*
*     Get current file
*
                  CHFILE = FILES(J)
                  LF = LENOCC(CHFILE)
                  CALL CLTOU(CHFILE(1:LF))
*
*     Get prefix
*
                  IBRA   = INDEXB(FILES(J),']')
                  PREFIX = FILES(J)(IBRA+1:IBRA+2)
*
*     Skip active files
*
                  IF(PREFIX.EQ.'ZZ') GOTO 100
*
*     Did it come from this node?
*
                  IF(INDEX(CHFILE(1:LF),'_'//CHHOST(1:LHOST)).EQ.0) THEN
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_SFS))
                     CALL CDUNIQ(LOCFIL,IRC)
                     LLOC = LENOCC(LOCFIL)
#endif
#if defined(CERNLIB_VAXVMS)
                     CHPATT = CHPATH(I)(1:LPATH(I)-1) // '.TODO' //
     +               CHPATH(I)(LPATH(I):LPATH(I))
                     LPATT = LENOCC(CHPATT)
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_WINNT))
                     CHPATT = CHPATH(I)(1:LPATH(I)) // '/TODO/'
                     LPATT = LENOCC(CHPATT)
                     CALL CUTOL(CHPATT(1:LPATT))
#endif
#if defined(CERNLIB_WINNT)
                     CHPATT = CHPATH(I)(1:LPATH(I)) // '\\TODO\\'
                     LPATT = LENOCC(CHPATT)
                     CALL CUTOL(CHPATT(1:LPATT))
#endif
#if defined(CERNLIB_IBMMVS)
                     CHPATT = CHPATH(I)(1:LPATH(I)) // '.TODO.'
                     LPATT = LENOCC(CHPATT)
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_SFS))
*
*     Receive into a temporary file
*
                     CHPATT = CHUSER(I)(1:LUSER(I)) // ' '
                     LPATT = LUSER(I) + 1
                     LOCFIL = CHNODE(I)(1:LNODE(I)) // ' A3'
                     LLOC = LNODE(I) + 3
#endif
                     WRITE(LPRTCD,9032) CHPATT(1:LPATT)//LOCFIL(1:LLOC),
     +                  CHFILE(1:LF)
 9032 FORMAT(' CDMOVE. receiving ',A,' from ',A)
                     CALL XZGETA(CHPATT(1:LPATT)//LOCFIL(1:LLOC),
     +               CHFILE(1 :LF),CHOPT,IRC)
                     IF(IRC.NE.0) THEN
                        ICONT = 0
                        GOTO 110
                     ENDIF
                     NGOT = NGOT + 1
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_SFS))
*
*     Now rename the local file
*
                     WRITE(LPRTCD,9026) CHPATT(1:LPATT)//LOCFIL(1:LLOC),
     +               CHPATT(1: LPATT)//PREFIX//LOCFIL(3:LLOC)
                     CALL XZLMV(CHPATT(1:LPATT)//LOCFIL(1:LLOC),
     +               CHPATT(1: LPATT)//PREFIX//LOCFIL(3:LLOC), ' ',IRC)
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_SFS))
*
*     Now send it to the local server
*     The server should not send it back if:
*     1) fromid   = gateway
*     2) fromnode = current node
*
                     IF(IDEBCD.GE.1)
     +               WRITE(LPRTCD,9033) CHPATT(1:LPATT)//LOCFIL(1:LLOC),
     +                  CHUSER(I)(1:LUSER(I)),CHHOST(1:LHOST)
 9033 FORMAT(' CDMOVE. sending ',A,' to ',A,' at ',A)
                     CALL VMCMS('EXEC SENDFILE '// CHPATT(1:LPATT)//
     +               LOCFIL(1:LLOC)// ' TO '//CHUSER(I)(1:LUSER(I))//
     +               ' AT '//CHHOST(1:LHOST),IRC)
 
#endif
                  ELSE
                     IF(IDEBCD.GE.1) WRITE(LPRTCD,9034) CHFILE(1:LF),
     +                  CHHOST(1:LHOST)
 9034 FORMAT(' CDMOVE. file ',A,' originated from this node (',A,')')
                  ENDIF
*
*     Delete
*
                  IF(IDEBCD.GE.1) WRITE(LPRTCD,9035) CHFILE(1:LF)
 9035 FORMAT(' CDMOVE. removing file ',A)
                  ISTART = 1
#if defined(CERNLIB_VAXVMS)
                  ISTART = INDEX(CHFILE(1:LF),']')+1
#endif
                  CALL XZRM(CHFILE(ISTART:LF),IRC)
 
  100          CONTINUE
               IF(ICONT.NE.0) GOTO 80
 
            ENDIF
  110       CONTINUE
            IF(IPROT.GE.0) THEN
*
*     Close current connection
*
               CALL CZCLOS(IRC)
            ENDIF
  120    CONTINUE
 
         IF(NUSERS.EQ.1) THEN
            IF(IDEBCD.GE.1) THEN
               CALL DATIME(ID,IT)
               WRITE(LPRTCD,9036) ID,IT,ISLEEP
 9036 FORMAT(' CDMOVE. time is ',I6,1X,I4,' sleeping for ',I6,
     +           ' seconds')
            ENDIF
 
            CALL SLEEPF(ISLEEP)
            GOTO 40
         ENDIF
  130 CONTINUE
 
      IF(IDEBCD.GE.1) THEN
         CALL DATIME(ID,IT)
         WRITE(LPRTCD,9036) ID,IT,ISLEEP
      ENDIF
 
      CALL SLEEPF(ISLEEP)
*
*     Reprocess names file in case of multiple groups
*
      GOTO 20
 
  140 CONTINUE
 
      CALL DATIME(ID,IT)
      WRITE(LPRTCD,9037) NSENT,NGOT,NUSERS
 9037 FORMAT(' CDMOVE. sent ',I6,' and received ',I6,
     +       ' files for ',I6,' servers')
      WRITE(LPRTCD,9038) ID,IT
 9038 FORMAT(' CDMOVE. stopping at ',I6,1X,I4)
 
#if defined(CERNLIB_VAXVMS)
*
*     Dummmies
*
      CALL CZDUMM
#endif
      END
#if defined(CERNLIB_VAXVMS)
      SUBROUTINE CZDUMM
      ENTRY CTL$GL_PCB
      ENTRY CTL$T_ACCOUNT
      ENTRY CTL$T_USERNAME
      END
#endif
