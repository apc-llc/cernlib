*
* $Id: cdconf.F,v 1.2 1996/03/12 13:07:56 cernlib Exp $
*
* $Log: cdconf.F,v $
* Revision 1.2  1996/03/12 13:07:56  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:35  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCONF(LUNNF,CHFILE,ILOGLV,IWAKE,IRC)
      CHARACTER*(*) CHFILE
      CHARACTER*20  CHIN(2,10)
      CHARACTER*255 CHOUT(2,20)
      CHARACTER*255 CHPREF,CHSERV,CHTEMP
#if defined(CERNLIB_VAXVMS)
      INCLUDE '($RMSDEF)'
#endif
#if defined(CERNLIB_UNIX)
      LOGICAL IEXIST
#endif
#include "hepdb/cdnamc.inc"
#include "hepdb/cduscm.inc"
*
*     Read configuration file
*
      IRC  = 0
      LF   = LENOCC(CHFILE)
      NIN  = 1
      NOUT = 8
*
*     Get list of databases, queue directory, log directory
*
      CHIN(1,1)  = ':nick'
      CHIN(2,1)  = 'config'
 
      CHOUT(1,1) = ':list'
      CHOUT(2,1) = ' '
 
      CHOUT(1,2) = ':log'
      CHOUT(2,2) = ' '
 
      CHOUT(1,3) = ':todo'
      CHOUT(2,3) = ' '
 
      CHOUT(1,4) = ':queue'
      CHOUT(2,4) = ' '
 
      CHOUT(1,5) = ':save'
      CHOUT(2,5) = ' '
 
      CHOUT(1,6) = ':bad'
      CHOUT(2,6) = ' '
 
      CHOUT(1,7) = ':loglevel'
      CHOUT(2,7) = ' '
 
      CHOUT(1,8) = ':wakeup'
      CHOUT(2,8) = ' '
 
      CALL NAMEFD(LUNNF,CHFILE(1:LF),CHIN,NIN,CHOUT,NOUT,IRC)
      IF(IRC.NE.0) THEN
         WRITE(LPRTCD,9001) IRC,CHFILE(1:LF)
 9001 FORMAT(' CDCONF. error ',I6,' processing names file ',A)
         WRITE(LPRTCD,9002)
 9002 FORMAT(' CDCONF. check that names file is correctly configured')
         WRITE(LPRTCD,9003)
 9003 FORMAT(' CDCONF. the following tags must be specified:')
         DO 10 I=1,8
            WRITE(LPRTCD,9004) CHOUT(1,I)(1:LENOCC(CHOUT(1,I)))
 9004 FORMAT(1X,A)
   10    CONTINUE
         RETURN
      ENDIF
*
*     Directories
*
      CDLOGD     = CHOUT(2,2)
      LLOGD      = LENOCC(CDLOGD)
      CDTODO     = CHOUT(2,3)
      LTODO      = LENOCC(CDTODO)
      CDQUED     = CHOUT(2,4)
      LQUED      = LENOCC(CDQUED)
      CDSAVD     = CHOUT(2,5)
      LSAVD      = LENOCC(CDSAVD)
      CDBADD     = CHOUT(2,6)
      LBADD      = LENOCC(CDBADD)
#if defined(CERNLIB_IBMVM)
      CALL CLTOU(CDSAVD(1:LSAVD))
      CALL CLTOU(CDBADD(1:LBADD))
#endif
*
*     Loglevel
*
      ILOGLV     = ICDECI(CHOUT(2,7),1,LEN(CHOUT(2,7)))
*
*     Wakeup interval in seconds
*
      IWAKE      = ICDECI(CHOUT(2,8),1,LEN(CHOUT(2,8)))
      IF(IWAKE.LE.0) THEN
         WRITE(LPRTCD,9005) IWAKE
 9005 FORMAT(' CDCONF. illegal wakeup interval of ',I6,
     +       ' seconds specified. Using default of 60')
         IWAKE = 60
      ENDIF
*
*     List of database prefixes
*
      CHPREF     = CHOUT(2,1)
      LPREF      = LENOCC(CHPREF)
#if defined(CERNLIB__DEBUG)
      WRITE(LPRTCD,9006) ILOGLV
 9006 FORMAT(' CDCONF. loglevel       : ',I6)
      WRITE(LPRTCD,9007) IWAKE
 9007 FORMAT(' CDCONF. wakeup interval: ',I6)
      WRITE(LPRTCD,9008) CDLOGD(1:LLOGD)
 9008 FORMAT(' CDCONF. log directory  : ',A)
      WRITE(LPRTCD,9009) CDQUED(1:LQUED)
 9009 FORMAT(' CDCONF. queue directory: ',A,' (client)')
      WRITE(LPRTCD,9010) CDTODO(1:LTODO)
 9010 FORMAT(' CDCONF. input directory: ',A,' (server)')
      IF(CDQUED(1:LQUED).EQ.CDTODO(1:LTODO)) THEN
         WRITE(LPRTCD,9011)
         MASTER = .TRUE.
      ELSE
         WRITE(LPRTCD,9012)
         MASTER = .FALSE.
      ENDIF
 9011 FORMAT(' CDCONF. client and server directories match -',
     +       ' operating as database master')
 9012 FORMAT(' CDCONF. client and server directories do not match -',
     +       ' operating as slave server')
      WRITE(LPRTCD,9013) CDSAVD(1:LSAVD)
 9013 FORMAT(' CDCONF. save directory : ',A)
      WRITE(LPRTCD,9014) CDBADD(1:LBADD)
 9014 FORMAT(' CDCONF. bad  directory : ',A)
      WRITE(LPRTCD,9015) CHPREF(1:LPREF)
 9015 FORMAT(' CDCONF. database codes : ',A)
#endif
*
*     Process list of database prefixes
*
      CALL CDNWRD(' ',CHPREF,NWORDS)
*
*     Number of servers
*
      NFILES = NWORDS
 
      IF(ILOGLV.GE.1) WRITE(LPRTCD,9016) NWORDS
 9016 FORMAT(' CDCONF. number of databases = ',I6)
 
      DO 40 I=1,NWORDS
 
         CALL CDWORD(CDPRFX(I),I-1,' ',CHPREF,IRC)
         CALL CLTOU(CDPRFX(I))
*
*     Get the file name and list of servers for this database
*
         NIN  = 1
         NOUT = 2
         CHIN(1,1)  = ':nick'
         CHIN(2,1)  = CDPRFX(I)
 
         CHOUT(1,1) = ':file'
         CHOUT(2,1) = ' '
 
         CHOUT(1,2) = ':servers'
         CHOUT(2,2) = ' '
 
         CALL NAMEFD(LUNNF,CHFILE(1:LF),CHIN,NIN,CHOUT,NOUT,IRC)
         IF(IRC.NE.0) THEN
            WRITE(LPRTCD,9017) IRC,CHFILE(1:LF),CDPRFX(I)
 9017    FORMAT(' CDCONF. error ',I6,' processing names file ',A,
     +          ' for entry ',A)
            WRITE(LPRTCD,9002)
            WRITE(LPRTCD,9003)
            DO 20 J=1,2
               WRITE(LPRTCD,9004) CHOUT(1,J)(1:LENOCC(CHOUT(1,J)))
   20       CONTINUE
            RETURN
         ENDIF
 
         CDFILE(I) = CHOUT(2,1)
         LDFILE(I) = LENOCC(CHOUT(2,1))
 
         IF(ILOGLV.GE.1) WRITE(LPRTCD,9018) CDPRFX(I),
     +      CDFILE(I)(1:LDFILE(I))
 9018 FORMAT(' CDCONF. database file for ',A,' is ',A)
*
*     Process the list of servers and get the local queue directories
*
         CHSERV     = CHOUT(2,2)
         CALL CDNWRD(' ',CHSERV,JWORDS)
         NSERV(I)   = JWORDS
 
         IF(ILOGLV.GE.1) WRITE(LPRTCD,9019) JWORDS
 9019 FORMAT(' CDCONF. number of remote servers = ',I6)
 
         CHIN(1,1)  = ':nick'
         CHOUT(1,1) = ':localq'
         CHOUT(2,1) = ' '
         NIN        = 1
         NOUT       = 1
 
         DO 30 J=1,JWORDS
            CALL CDWORD(CHIN(2,1),J-1,' ',CHSERV,IRC)
            IF(ILOGLV.GE.2) PRINT *,'CDCONF. processing ',CHIN(2,1)
            CALL NAMEFD(LUNNF,CHFILE(1:LF),CHIN,NIN,CHOUT,NOUT,IRC)
            IF(IRC.NE.0) THEN
               WRITE(LPRTCD,9020) IRC,CHFILE(1:LF),CHIN(2,1)
 9020 FORMAT(' CDCONF. error ',I6,' processing names file ',A,
     +       ' for entry ',A)
               WRITE(LPRTCD,9021)
 9021 FORMAT(' CDCONF. is there a :LOCALQ tag for this entry?')
               RETURN
            ENDIF
            CDREMQ(I,J) = CHOUT(2,1)
            LREMQ (I,J) = LENOCC(CHOUT(2,1))
            IF(ILOGLV.GE.2) WRITE(LPRTCD,9022) CHIN(2,1),
     +         CDREMQ(I,J)(1:LREMQ(I,J))
 9022 FORMAT(' CDCONF. local q for ',A,' is ',A)
*
*     Make queue directories if they don't exist
*
#if defined(CERNLIB_VAXVMS)
            ISTAT = LIB$FIND_FILE(CDREMQ(I,J)(1:LREMQ(I,J)),CHTEMP,
     +      JCONT)
            IF(ISTAT.EQ.RMS$_DNF) THEN
               IF(ILOGLV.GE.0) WRITE(LPRTCD,9023) CHIN(2,1), CDREMQ(I,
     +         J)(1:LREMQ(I,J))
               ISTAT = LIB$CREATE_DIR(CDREMQ(I,J)(1:LREMQ(I,J)),,,,,)
               IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
            ENDIF
            ISTAT = LIB$FIND_FILE_END(JCONT)
#endif
#if defined(CERNLIB_UNIX)
            INQUIRE(FILE=CDREMQ(I,J)(1:LREMQ(I,J)),EXIST=IEXIST)
            IF(.NOT.IEXIST) THEN
               IF(ILOGLV.GE.0) WRITE(LPRTCD,9023) CHIN(2,1), CDREMQ(I,
     +         J)(1:LREMQ(I,J))
               ISTAT = SYSTEMF('mkdir '//CDREMQ(I,J)(1:LREMQ(I,J)))
            ENDIF
#endif
 9023 FORMAT(' CDCONF. creating local q for ',A,' (',A,')')
   30    CONTINUE
   40 CONTINUE
      END
