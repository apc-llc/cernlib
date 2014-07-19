*
* $Id: fatsend.F,v 1.3 1996/04/12 07:55:56 cernlib Exp $
*
* $Log: fatsend.F,v $
* Revision 1.3  1996/04/12 07:55:56  cernlib
* new handling of title string
*
* Revision 1.2  1996/03/29 11:30:00  jamie
* qftitlch
*
* Revision 1.1.1.1  1996/03/07 15:17:40  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PROGRAM FATSEND
*******************************************************************************
*                                                                             *
*           ######    ##     #####   ####   ######  #    #  #####             *
*           #        #  #      #    #       #       ##   #  #    #            *
*           #####   #    #     #     ####   #####   # #  #  #    #            *
*           #       ######     #         #  #       #  # #  #    #            *
*           #       #    #     #    #    #  #       #   ##  #    #            *
*           #       #    #     #     ####   ######  #    #  #####             *
*                                                                             *
*******************************************************************************
*                                                                             *
*     Send FATMEN updates to remote servers                                   *
*     This version is for VM, Unix and VAX/VMS systems only                   *
*     (MVS version requires XZLLS, GETENVF)                                   *
*     Can be used to transfer updates to MAXSRV remote machines               *
*     Updates can be for different groups.                                    *
*                                                                             *
*     Method:  FATSEND uses environment variables to determine which          *
*     =======  groups to process                                              *
*              FATGROUPS = comma delimited list of groups                     *
*                e.g. export FATGROUPS="FMDELPHI,FML3,FMCNDIV"                *
*              FATGRP    = single group                                       *
*                e.g. export FATGRP="FML3"                                    *
*                same as setting FATGROUPS to FML3                            *
*                                                                             *
*              The names file of each group is processed in turn              *
*              and the updates moved from the local to remote machine         *
*                                                                             *
*              The method is not optimal in the sense that it connects        *
*              to each machine for each group in turn                         *
*                                                                             *
*******************************************************************************
 
      PARAMETER     (MAXFIL=1000)
      PARAMETER     (MAXSRV=50)
      PARAMETER     (MAXGRP=50)
      PARAMETER     (IPRINT=6)
      PARAMETER     (IDEBUG=-1)
      PARAMETER     (LUNI=1)
      PARAMETER     (LUNO=2)
      COMMON/PAWC/PAW(50000)
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
#include "zebra/quest.inc"
      COMMON/CZSOCK/LUNCZ,IADTCP,LBUF,ISKIN,ISKOUT,IPROT
      CHARACTER*255 FILES(MAXFIL),CHFILE,CHPATT,CHDIR
      CHARACTER*20  CHIN(2,10)
      CHARACTER*255 CHOUT(2,20)
      CHARACTER*255 CHSERV,CHLIST,CHSTOP
      CHARACTER*255 CHPATH(MAXSRV)
      CHARACTER*255 GENAM,REMDIR,REMFIL,LOCFIL,TODO
      CHARACTER*20  CHTEMP
      CHARACTER*80  CHCOMM
      CHARACTER*80  CHMAIL
      CHARACTER*8   CHWAKE,CHLOGL,USER,CHHOST,CHTYPE,CHSYS
      CHARACTER*8   GROUP
      CHARACTER*8   CHGRP (MAXGRP)
      CHARACTER*20  CHNAME(MAXSRV)
      CHARACTER*20  CHUSER(MAXSRV)
      CHARACTER*80  CHNODE(MAXSRV)
      CHARACTER*20  CHPROT(MAXSRV)
      CHARACTER*20  CHRECV(MAXSRV)
      CHARACTER*80  CHQUED(MAXSRV)
      CHARACTER*1   CHMODE,CHOPT
      INTEGER*4     FMUSER,FMHOST
      DIMENSION     LGRP (MAXGRP)
      DIMENSION     LPATH(MAXSRV),LNAME(MAXSRV),LUSER(MAXSRV),
     +              LNODE(MAXSRV),LPROT(MAXSRV),
     +              LRECV(MAXSRV),LQUED(MAXSRV)
      LOGICAL       IEXIST,IOPEN,IWAIT
      CHARACTER    VIDQQ*(*)
#include "fatmen/qftitlch.inc"
      PARAMETER   (VIDQQ = '@(#)' //
     + FatmenTitleFortranString
     + // '>')
      CALL CLEFT (VIDQQ,1,0)
*
*     Initialise ZEBRA
*
      CALL HLIMIT(50000)
*
*     Initialise XZ
*
      CALL XZINIT(IPRINT,IDEBUG,LUNI,LUNO)
*
*     Get host name
*
      IC = FMHOST(CHHOST,CHTYPE,CHSYS)
      LHOST = LENOCC(CHHOST)
      CALL CLTOU(CHHOST(1:LHOST))
#if defined(CERNLIB_UNIX)
*
*     Get process ID
*
      CALL GETPIDF(IPID)
      IF(CHHOST(1:LHOST).EQ.'FATCAT'.OR.
     +   CHHOST(1:LHOST).EQ.'SP020') THEN
         WRITE(CHCOMM,9001) IPID
 9001 FORMAT(' echo PID = ',I10,' >> /fatmen/FMSEND.log')
         ICODE = SYSTEMF(CHCOMM)
         WRITE(LPRTFA,9002) IPID
 9002 FORMAT(' FATSEND. process ID is ',I10)
      ENDIF
#endif
*
*     Get list of users to process
*
      CALL GETENVF('FATGROUPS',CHLIST)
      LLIST = IS(1)
      IF(LLIST.EQ.0) THEN
         NUSERS = 1
*
*     Get our name
*
         CALL GETENVF('FATGRP',CHGRP(1))
         LGRP(1) = IS(1)
         IF(IS(1).EQ.0) THEN
            IC = FMUSER(CHGRP(1))
            LGRP(1) = LENOCC(CHGRP(1))
         ENDIF
         CALL CLTOU(CHGRP(1)(1:LGRP(1)))
      ELSE
         CALL CLTOU(CHLIST(1:LLIST))
         CALL FMNWRD(',',CHLIST(1:LLIST),NUSERS)
         DO 10  I=1,NUSERS
            CALL FMWORD(CHGRP(I),I-1,',',CHLIST(1:LLIST),IRC)
            LGRP(I) = LENOCC(CHGRP(I))
   10    CONTINUE
      ENDIF
*
*     Get stop file
*
      CALL GETENVF('FATSTOP',CHSTOP)
      LSTOP = IS(1)
*
*     Initialise counters
*
      NSENT = 0
      NGOT  = 0
*
*     Wait unless no files were transferred
*
      IWAIT = .TRUE.
*
*     Get the log level
*
      CALL GETENVF('FMLOGL',CHLOGL)
      IF(IS(1).GT.0) THEN
         IDEBFA = ICDECI(CHLOGL,1,8)
      ELSE
         IDEBFA = 0
      ENDIF
 
      IF(IDEBFA.GT.0) THEN
         CHOPT = 'S'
      ELSE
         CHOPT = ' '
      ENDIF
*
*     Get the wakeup interval
*
      CALL GETENVF('FMWAKEUP',CHWAKE)
      IF(IS(1).GT.0) THEN
         ISLEEP = ICDECI(CHWAKE,1,8)
      ELSE
         ISLEEP = 60
      ENDIF
 
      NPASS = 0
 
   20 CONTINUE
 
      NPASS = NPASS + 1
 
      DO 130 N=1,NUSERS
 
         IF(IDEBFA.GE.0) PRINT 9003,CHGRP(N)(1:LGRP(N))
 9003 FORMAT(' FATSEND. processing group ',A)
 
         IF(CHGRP(N)(1:2).EQ.'FM') THEN
            GROUP = CHGRP(N)(1:LGRP(N))
         ELSE
            GROUP = 'FM'//CHGRP(N)(1:LGRP(N))
         ENDIF
*
*     Get location of names file
*
         CALL GETENVF(GROUP(1:LGRP(N)),CHDIR)
         LDIR = IS(1)
         IF(LDIR.EQ.0) THEN
            CHFILE = GROUP(1:LGRP(N))//'.NAMES'
         ELSE
#if defined(CERNLIB_IBMMVS)
            CHFILE = CHDIR(1:LDIR)//'.'//
     +         GROUP(1:LGRP(N))//'.NAMES'
#endif
#if defined(CERNLIB_VAXVMS)
            CHFILE = CHDIR(1:LDIR)//
     +         GROUP(1:LGRP(N))//'.NAMES'
#endif
#if defined(CERNLIB_UNIX)
            CHFILE = CHDIR(1:LDIR)//'/'//
     +         GROUP(1:LGRP(N))//'.NAMES'
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
         CHIN(1,1) = ':nick'
         CHIN(2,1) = 'FATSEND'
 
         CHOUT(1,1) = ':wakeup'
         CHOUT(2,1) = ' '
         CHOUT(1,2) = ':logl'
         CHOUT(2,2) = ' '
 
         NIN = 1
         NOUT = 2
         LUN = 1
         CALL NAMEFD(LUN,CHFILE(1:LF),CHIN,NIN,CHOUT,NOUT,IRC)
         IF(IRC.EQ.0) THEN
            LOUT = LENOCC(CHOUT(2,1))
            IF(LOUT.NE.0) ISLEEP = ICDECI(CHOUT(2,1),1,LOUT)
            LOUT = LENOCC(CHOUT(2,2))
            IF(LOUT.NE.0) IDEBFA = ICDECI(CHOUT(2,1),1,LOUT)
         ENDIF
#endif
*
*     Get the list of FATSERVERS...
*
         CHIN(1,1) = ':nick'
         CHIN(2,1) = 'FATSERVERS'
 
         CHOUT(1,1) = ':list'
 
         NIN = 1
         NOUT = 1
         LUN = 1
         IF(IDEBFA.GE.1) PRINT 9004,CHFILE(1:LF)
 9004 FORMAT(' FATSEND. processing names file ',A)
         CALL NAMEFD(LUN,CHFILE(1:LF),CHIN,NIN,CHOUT,NOUT,IRC)
         IF(IRC.NE.0) THEN
            PRINT *,'FATSEND. error from NAMEFD, IRC = ',IRC
            GOTO 140
         ENDIF
*
*     For each server, get
*        :node
*        :userid
*
*     :userid gives us the name of the FATMEN group for whom
*     we are working. e.g.
*     :userid.fmcdf
*     We expect an environmental variable/symbol of this
*     name (uppercase), e.g.
*     FMCDF:==USR$ROOT37:[FMCDF]
*     This has subdirectories [.TOnode], e.g.
*     :node.FNALF = [.TOFNALF]
*
         CHSERV = CHOUT(2,1)
         CALL CSQMBL(CHSERV,1,LEN(CHSERV))
         LCHSERV = LENOCC(CHSERV)
 
         CALL FMNWRD(' ',CHSERV(1:LCHSERV),NSERV)
 
         IF(NSERV.GT.MAXSRV) THEN
            PRINT *,'FATSEND. cannot process more than ',MAXSRV,
     +      ' servers'
            NSERV = MAXSRV
         ENDIF
 
         DO 30 I=1,NSERV
            CALL FMWORD(CHNAME(I),I-1,' ',CHSERV(1:LCHSERV),IRC)
            IF(IDEBFA.GE.0) PRINT 9005,CHNAME(I)
 9005 FORMAT(' FATSEND. processing node ',A)
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
#if defined(CERNLIB_IBMMVS)
            CHOUT(1,6) = ':mvsid'
            CHOUT(2,6) = ' '
#endif
 
            NIN = 1
#if defined(CERNLIB_IBMMVS)
            NOUT = 6
#endif
#if !defined(CERNLIB_IBMMVS)
            NOUT = 5
#endif
 
            CALL NAMEFD(LUN,CHFILE(1:LF),CHIN,NIN,CHOUT,NOUT,IRC)
 
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
#if defined(CERNLIB_IBMMVS)
            LPATH(I) = LENOCC(CHOUT(2,6))
            CHPATH(I) = CHOUT(2,6)(1:LPATH(I))
            CALL CLTOU(CHPATH(I)(1:LPATH(I)))
#endif
 
            IF(NPASS.EQ.1.AND.IDEBFA.GE.0) THEN
               PRINT 9006,CHUSER(I)(1:LUSER(I))
 9006 FORMAT(' FATSEND. remote user  : ',A)
               PRINT 9007,CHNODE(I)(1:LNODE(I))
 9007 FORMAT(' FATSEND. remote node  : ',A)
               IF(CHPROT(I)(1:LPROT(I)).EQ.'afs') THEN
                  PRINT 9008,CHPROT(I)(1:LPROT(I))
 9008 FORMAT(' FATSEND. protocol     : ',A,' (skipped by this server)')
               ELSE
                  PRINT 9009,CHPROT(I)(1:LPROT(I))
 9009 FORMAT(' FATSEND. protocol     : ',A)
               ENDIF
               PRINT 9010,CHRECV(I)(1:LRECV(I))
 9010 FORMAT(' FATSEND. receive?     : ',A)
               PRINT 9011,CHQUED(I)(1:LQUED(I))
 9011 FORMAT(' FATSEND. remote queue : ',A)
            ENDIF
 
            IF(CHRECV(I)(1:LRECV(I)).EQ.'YES') THEN
               LRECV(I) = 0
            ELSE
               IF(INDEX(CHPROT(I)(1:LPROT(I)),'BITNET').NE.0) THEN
                  IF(IDEBFA.GE.-3) PRINT *,'FATSEND. receive option ',
     +            'invalid for Bitnet nodes'
                  LRECV(I) = 0
               ENDIF
            ENDIF
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_SFS))
*
*     Get disk mode on which the queue files are kept
*
            CALL GETENVF('FATQUE',CHMODE)
            IF(IS(1).EQ.0) THEN
               IF(NPASS.EQ.1.AND.IDEBFA.GE.0)
     +         PRINT *,'FATSEND. queue disk defaulted to B'
               CHMODE = 'B'
            ELSE
               IF(NPASS.EQ.1.AND.IDEBFA.GE.0)
     +         PRINT *,'FATSEND. queue disk is at mode ',CHMODE
            ENDIF
 
#endif
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_SFS)||!defined(CERNLIB_IBMMVS))
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
               IF(NPASS.EQ.1)
     +         PRINT *,'FATSEND. error - variable ',CHUSER(I)(1:
     +         LUSER(I)), ' is not defined. Updates will not be '
     +         //'processed.'
            ELSE
               IF(NPASS.EQ.1.AND.IDEBFA.GE.0)
     +            PRINT *,'FATSEND. local queue : ',
     +            CHPATH(I) (1:LPATH(I))
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
*
*     Skip if protocol is AFS
*
            IF(CHPROT(I)(1:LPROT(I)).EQ.'afs') GOTO 120
#endif
*
*     Look for files in local queue
*
#if defined(CERNLIB_VAXVMS)
            CHPATT = CHPATH(I)(1:LPATH(I)-1) // '.TO' // CHNODE(I)(1:
     +      LNODE(I)) // CHPATH(I)(LPATH(I):LPATH(I))
            LPATT = LENOCC(CHPATT)
#endif
#if defined(CERNLIB_UNIX)
            CHPATT = CHPATH(I)(1:LPATH(I)) // '/TO' // CHNODE(I)(1:
     +      LNODE(I))
            LPATT = LENOCC(CHPATT)
            CALL CUTOL(CHPATT(1:LPATT))
#endif
#if defined(CERNLIB_IBMMVS)
            CHPATT = CHPATH(I)(1:LPATH(I)) // '.TO' // CHNODE(I)(1:
     +      LNODE(I))
            LPATT = LENOCC(CHPATT)
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_SFS))
*
*     Look for files on the FATQUE disk
*
            CHPATT = CHUSER(I)(1:LUSER(I)) // ' ' // CHNODE(I)(1:
     +      LNODE(I)) // ' ' //CHMODE
            LPATT = LENOCC(CHPATT)
#endif
#if !defined(CERNLIB_IBMVM)
*
*     Look for signal.stop file
*
            IF(LSTOP.EQ.0) THEN
 
#endif
#if defined(CERNLIB_IBMMVS)
               CHSTOP = '/'//CHPATT(1:LPATT)//'.SIGNAL.STOP'
               LSTOP = LPATT + 13
#endif
#if defined(CERNLIB_UNIX)
               CHSTOP = CHPATT(1:LPATT)//'/signal.stop'
               LSTOP = LPATT + 12
#endif
#if defined(CERNLIB_VAXVMS)
               CHSTOP = CHPATT(1:LPATT)//'SIGNAL.STOP'
               LSTOP = LPATT + 11
#endif
#if !defined(CERNLIB_IBMVM)
            ENDIF
            INQUIRE(FILE=CHSTOP(1:LSTOP),EXIST=IEXIST)
            IF(IEXIST) THEN
               PRINT *,'FATSEND. signal.stop file found - stopping'
               GOTO 140
            ENDIF
#endif
 
            ICONT = 0
            ISENT = 0
            IOPEN = .FALSE.
   50       CONTINUE
            CALL XZLLS(CHPATT(1:LPATT),FILES,MAXFIL,NFOUND,ICONT,' ',
     +      IRC)
            IF(NFOUND.EQ.0) GOTO 120
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
            CALL CZOPEN('zserv',CHNODE(I)(1:LNODE(I)),IRC)
            IF(IRC.NE.0) THEN
               PRINT *,'FATSEND. cannot connect to ', CHNODE(I)(1:
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
*
*     If remote queue is not defined, assume that we are
*     in home directory of remote server
*
            IF(LQUED(I).EQ.0) THEN
               IF(IVAX.EQ.1) THEN
                  IF(IDEBFA.GE.1) PRINT 9012,'[.TODO]'
                  CALL XZCD('[.TODO]',IRC)
               ELSE
                  IF(IDEBFA.GE.1) PRINT 9012,'TODO'
                  CALL XZCD('TODO',IRC)
               ENDIF
            ELSE
               IF(IDEBFA.GE.1) PRINT 9012,CHQUED(I)(1:LQUED(I))
               CALL XZCD(CHQUED(I)(1:LQUED(I)),IRC)
            ENDIF
 9012 FORMAT(' FATSEND. setting remote directory to ',A)
 
            IF(IRC.NE.0) GOTO 140
 
   60       CONTINUE
            IOPEN = .TRUE.
 
            DO 70 J=1,NFOUND
#if defined(CERNLIB_UNIX)
               CHFILE = CHPATT(1:LPATT)//'/'//FILES(J)
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_IBMMVS)
               CHFILE = FILES(J)
#endif
               LF = LENOCC(CHFILE)
*
*     Skip active files
*
#if defined(CERNLIB_VAXVMS)
               IBRA = INDEXB(CHFILE(1:LF),']')
               IF(CHFILE(IBRA+1:IBRA+2).EQ.'ZZ') GOTO 70
               IF(CHFILE(IBRA+1:IBRA+2).EQ.'zz') GOTO 70
#endif
#if defined(CERNLIB_UNIX)
               ISLA = INDEXB(CHFILE(1:LF),'/')
               IF(CHFILE(ISLA+1:ISLA+2).EQ.'ZZ') GOTO 70
               IF(CHFILE(ISLA+1:ISLA+2).EQ.'zz') GOTO 70
#endif
*
*     Transfer the file
*
               IF(IPROT.LT.0) THEN
                  CALL FABNET(CHFILE(1:LF),CHUSER(I)(1:LUSER(I)),
     +            CHNODE(I )(1:LNODE(I)),IPROT,IRC)
               ELSE
                  CALL FMJOUR(REMFIL)
                  LREM = LENOCC(REMFIL)
                  IF(IDEBFA.GE.1) PRINT 9013,CHFILE(1:LF),
     +                                       REMFIL(1:LREM)
 9013 FORMAT(' FATSEND. sending ',A,' as ',A)
                  CALL XZPUTA(CHFILE(1:LF),REMFIL(1:LREM),CHOPT,IRC)
                  IF(IRC.EQ.0) THEN
                     NSENT = NSENT + 1
                     ISENT = ISENT + 1
*
*     Now rename the remote file
*
                     IF(IDEBFA.GE.1) PRINT 9014,REMFIL(1:LREM),
     +                                          'AA'//REMFIL(3:LREM)
 9014 FORMAT(' FATSEND. renaming ',A,' to ',A)
                     CALL XZMV(REMFIL(1:LREM),'AA'//REMFIL(3:LREM),' ',
     +               IRC)
                  ENDIF
               ENDIF
               IF(IRC.NE.0) THEN
                  PRINT 9015,CHFILE(1:LF),CHNODE(I)(1:LNODE(I))
 9015 FORMAT(' FATSEND. error transferring ',A,' to ',A)
                  ICONT = 0
                  GOTO 110
               ENDIF
*
*     and delete the original if successful
*
               IF(IDEBFA.GE.1) PRINT 9016,CHFILE(1:LF)
 9016 FORMAT(' FATSEND. deleting ',A)
               CALL XZLRM(CHFILE(1:LF),IRC)
*
*     any remaining files to process?
*
   70       CONTINUE
            IF(ICONT.NE.0) GOTO 50
 
            IF(ISENT.NE.0) IWAIT = .FALSE.
            IF(ISENT.GT.0.AND.IDEBFA.GE.0) THEN
               CALL DATIME(ID,IT)
               WRITE(LPRTFA,9017) ID,IT,ISENT,CHNODE(I)(1:LNODE(I)),
     +                            CHGRP(N)(1:LGRP(N))
 9017 FORMAT(' FATSEND. ',I6,1X,I4,' sent ',I10,' files to ',A,
     +       ' for ',A)
            ENDIF
*
*     Should we receive any files for us?
*
            IF(LRECV(I).EQ.0) THEN
               IF(IVAX.EQ.0) THEN
                  IF(IDEBFA.GE.1) PRINT 9012,
     +               '../TO'//CHHOST(1:LHOST)
                  CALL XZCD('../TO'//CHHOST(1:LHOST),IRC)
               ELSE
                  IF(IDEBFA.GE.1) PRINT 9012,
     +               '[-.TO'//CHHOST(1:LHOST)//']'
                  CALL XZCD('[-.TO'//CHHOST(1:LHOST)//']',IRC)
               ENDIF
               IF(IRC.NE.0) GOTO 110
*
*        Anything there?
*
               ICONT = 0
               IGOT  = 0
   80          CONTINUE
               CALL XZLS(' ',FILES,MAXFIL,NFOUND,ICONT,' ',IRC)
               IF(ICONT.NE.0) THEN
                  PRINT *,'FATSEND. too many files - excess names '//
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
*     Skip active files
*
                  IBRA = INDEXB(CHFILE(1:LF),']')
                  IF(CHFILE(IBRA+1:IBRA+2).EQ.'ZZ') GOTO 100
*
*     Did it come from this node?
*
                  IF(INDEX(CHFILE(1:LF),'_'//CHHOST(1:LHOST)).EQ.0) THEN
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_SFS))
                     CALL FMJOUR(LOCFIL)
                     LLOC = LENOCC(LOCFIL)
#endif
#if defined(CERNLIB_VAXVMS)
                     CHPATT = CHPATH(I)(1:LPATH(I)-1) // '.TODO' //
     +               CHPATH(I)(LPATH(I):LPATH(I))
                     LPATT = LENOCC(CHPATT)
#endif
#if defined(CERNLIB_UNIX)
                     CHPATT = CHPATH(I)(1:LPATH(I)) // '/TODO/'
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
                     PRINT 9018,CHPATT(1:LPATT)//LOCFIL(1:LLOC),
     +                  CHFILE(1:LF)
 9018 FORMAT(' FATSEND. receiving ',A,' from ',A)
                     CALL XZGETA(CHPATT(1:LPATT)//LOCFIL(1:LLOC),
     +               CHFILE(1 :LF),CHOPT,IRC)
                     IF(IRC.NE.0) THEN
                        ICONT = 0
                        GOTO 110
                     ENDIF
                     NGOT = NGOT + 1
                     IGOT = IGOT + 1
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_SFS))
*
*     Now rename the local file
*
                     PRINT 9014,CHPATT(1:LPATT)//LOCFIL(1:LLOC),
     +               CHPATT(1: LPATT)//'AA'//LOCFIL(3:LLOC)
                     CALL XZLMV(CHPATT(1:LPATT)//LOCFIL(1:LLOC),
     +               CHPATT(1: LPATT)//'AA'//LOCFIL(3:LLOC), ' ',IRC)
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_SFS))
*
*     Now send it to the local server
*     The FATMEN server should not send it back if:
*     1) fromid   = gateway
*     2) fromnode = current node
*
                     IF(IDEBFA.GE.1)
     +               PRINT 9019,CHPATT(1:LPATT)//LOCFIL(1:LLOC),
     +                  CHUSER(I)(1:LUSER(I)),CHHOST(1:LHOST)
 9019 FORMAT(' FATSEND. sending ',A,' to ',A,' at ',A)
                     CALL VMCMS('EXEC SENDFILE '// CHPATT(1:LPATT)//
     +               LOCFIL(1:LLOC)// ' TO '//CHUSER(I)(1:LUSER(I))//
     +               ' AT '//CHHOST(1:LHOST),IRC)
 
#endif
                  ELSE
                     IF(IDEBFA.GE.1) PRINT 9020,CHFILE(1:LF),
     +                  CHHOST(1:LHOST)
 9020 FORMAT(' FATSEND. file ',A,' originated from this node (',A,')')
                  ENDIF
*
*     Delete
*
                  IF(IDEBFA.GE.1) PRINT 9021,CHFILE(1:LF)
 9021 FORMAT(' FATSEND. removing file ',A)
                  CALL XZRM(CHFILE(1:LF),IRC)
 
  100          CONTINUE
               IF(ICONT.NE.0) GOTO 80
 
            IF(IGOT.NE.0) IWAIT = .FALSE.
            IF(IGOT.GT.0.AND.IDEBFA.GE.0) THEN
               CALL DATIME(ID,IT)
               WRITE(LPRTFA,9022) ID,IT,IGOT,CHNODE(I)(1:LNODE(I)),
     +                            CHGRP(N)(1:LGRP(N))
 9022 FORMAT(' FATSEND. ',I6,1X,I4,' received ',I10,' files from ',A,
     +       ' for ',A)
            ENDIF
 
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
            IF(IWAIT) THEN
               IF(IDEBFA.GE.0) THEN
                  CALL DATIME(ID,IT)
                  PRINT 9023,ID,IT,ISLEEP
 9023 FORMAT(' FATSEND. time is ',I6,1X,I4,' sleeping for ',I6,
     +           ' seconds')
               ENDIF
               IF(IDEBFA.GE.2) PRINT 9024
 9024 FORMAT(' FATSEND. (no files transferred in last iteration)')
 
               CALL SLEEPF(ISLEEP)
            ENDIF
            IWAIT = .TRUE.
            GOTO 40
         ENDIF
  130 CONTINUE
 
      IF(IWAIT) THEN
         IF(IDEBFA.GE.0) THEN
            CALL DATIME(ID,IT)
            PRINT 9023,ID,IT,ISLEEP
         ENDIF
         IF(IDEBFA.GE.2) PRINT 9024
 
         CALL SLEEPF(ISLEEP)
      ENDIF
      IWAIT = .TRUE.
*
*     Reprocess names file in case of multiple groups
*
      GOTO 20
 
  140 CONTINUE
 
      CALL DATIME(ID,IT)
      PRINT 9025,NSENT,NGOT,NUSERS
 9025 FORMAT(' FATSEND. sent ',I6,' and received ',I6,
     +       ' files for ',I6,' servers')
      PRINT 9026,ID,IT
 9026 FORMAT(' FATSEND. stopping at ',I6,1X,I4)
 
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
