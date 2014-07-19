*
* $Id: cdlls.F,v 1.2 1996/03/08 16:40:39 jamie Exp $
*
* $Log: cdlls.F,v $
* Revision 1.2  1996/03/08 16:40:39  jamie
* Fix array bound problem for VMS
*
* Revision 1.1.1.1  1996/02/28 16:24:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLLS(LUNCDI,CHPATT,FILES,NMAX,NFOUND,ICONT,IRC)
      CHARACTER*(*) FILES(NMAX)
      CHARACTER*(*) CHPATT
      CHARACTER*255 CHLIST
      CHARACTER*255 CHFILE,CHDEF
      CHARACTER*255 COMM
      CHARACTER*255 CHLOG,CHERR
      CHARACTER*255 CHCWD
      COMMON/SLATE/IS(40)
      INTEGER       SYSTEMF
      SAVE          NDONE
*
#if defined(CERNLIB_VAXVMS)
      INCLUDE '($RMSDEF)'
      INTEGER SYS$GETMSG
      CHARACTER*255 CHDIR
#endif
#if defined(CERNLIB_UNIX)
      LOGICAL       IEXIST
#endif
*
*     Return files matching pattern CHPATT
*
      IRC    = 0
      LPATT  = LENOCC(CHPATT)
      IF(LPATT.EQ.0) THEN
#if defined(CERNLIB_IBM)||defined(CERNLIB_UNIX)
         LPATT  = 1
         CHLIST = ' '
      ELSE
         CHLIST = CHPATT(1:LPATT)
#endif
#if defined(CERNLIB_VAXVMS)
         LPATT  = 3
         CHLIST = '*.*'
      ELSE
         CHLIST = CHPATT(1:LPATT)
         IF((CHPATT(LPATT:LPATT).EQ.':').OR.
     +      (CHPATT(LPATT:LPATT).EQ.'>').OR.
     +      (CHPATT(LPATT:LPATT).EQ.']')) THEN
             CHLIST(LPATT+1:LPATT+3) = '*.*'
             LPATT = LPATT + 3
         ENDIF
#endif
      ENDIF
#if defined(CERNLIB_UNIX)
      CALL CUTOL(CHLIST)
#endif
      NFOUND = 0
      ICONT  = 0
      COMM   = ' '
#if defined(CERNLIB_UNIX)
      CALL GETWDF(CHCWD)
      LCWD = IS(1)
      CHLOG = CHCWD(1:LCWD)//'/hepdb.tmp'
      CHERR = CHCWD(1:LCWD)//'/hepdb.err'
      LLOG  = LENOCC(CHLOG)
      LERR  = LLOG
      WRITE(COMM,9001) CHLIST(1:LPATT),CHLOG(1:LLOG),CHERR(1:LERR)
 9001 FORMAT('ls ',A,' >',A,' 2>',A)
      IC = SYSTEMF(COMM)
      INQUIRE(FILE=CHLOG(1:LLOG),EXIST=IEXIST)
      IF(.NOT.IEXIST) THEN
         IRC  = -2
         GOTO 999
      ENDIF
      OPEN(LUNCDI,FILE=CHLOG(1:LLOG),FORM='FORMATTED',STATUS='OLD')
      ISKIP = 0
      IF(ICONT.EQ.0) NDONE = 0
   10 CONTINUE
      READ(LUNCDI,'(A)',END=20) CHFILE
      ISKIP = ISKIP + 1
      IF(ISKIP.LT.NDONE) THEN
         GOTO 10
      ENDIF
      NFOUND = NFOUND + 1
      IF(NFOUND.GT.NMAX) THEN
         NFOUND = NMAX
         IRC    = -1
         ICONT  = 1
         GOTO 20
      ENDIF
      FILES(NFOUND) = CHFILE(1:LENOCC(CHFILE))
      GOTO 10
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_APOLLO))
   20 CLOSE(LUNCDI,STATUS='DELETE')
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_APOLLO))
   20 CLOSE(LUNCDI)
#endif
#if defined(CERNLIB_VAXVMS)
      CHDEF  = ' '
      CALL GETDEF(CHDEF)
      LDEF = LENOCC(CHDEF)
   10 CONTINUE
      CHFILE = ' '
      IF ((LDEF.NE.0) .AND.
     +   (INDEX(CHPATT(1:LPATT),':').EQ.0).AND.
     +   (INDEX(CHPATT(1:LPATT),'>').EQ.0).AND.
     +   (INDEX(CHPATT(1:LPATT),'-').EQ.0)) THEN
         ISTAT = LIB$FIND_FILE(CHDEF(1:LDEF)//
     +                         CHLIST(1:LPATT),CHFILE,ICONT)
         ELSE
         ISTAT = LIB$FIND_FILE(CHLIST(1:LPATT),CHFILE,ICONT)
         ENDIF
*
*     Check for RMS$_DNF - directory not found
*               RMS$_FNF - file      not found
*               RMS$_NMF - no more files
*
      IF(ISTAT.EQ.RMS$_DNF) THEN
         IRC = ISTAT
         CALL LIB$FIND_FILE_END(ICONT)
         ICONT = 0
         GOTO 999
      ELSEIF(ISTAT.EQ.RMS$_FNF) THEN
         CALL LIB$FIND_FILE_END(ICONT)
         ICONT = 0
         GOTO 999
      ELSEIF(ISTAT.EQ.RMS$_NMF) THEN
         CALL LIB$FIND_FILE_END(ICONT)
         ICONT = 0
         GOTO 999
      ENDIF
 
      IF(.NOT.ISTAT) THEN
         IRC = SYS$GETMSG(%VAL(ISTAT),LFILE,CHFILE,,)
         PRINT 9002,CHFILE(1:LFILE)
 9002 FORMAT(' CDLLS. error from LIB$FIND_FILE = ',A)
         IRC = ISTAT
         ISTAT = LIB$FIND_FILE_END(ICONT)
         GOTO 999
      ELSE
         NFOUND = NFOUND + 1
         IF(NFOUND.GT.NMAX) THEN
            NFOUND = NMAX
            IRC    = -1
            GOTO 999
         ENDIF
         FILES(NFOUND) = CHFILE(1:LENOCC(CHFILE))
         GOTO 10
      ENDIF
 
#endif
#if defined(CERNLIB_IBMVM)
      IF(ICONT.EQ.0) NDONE = 0
      CALL CTRANS('.',' ',CHPATT,1,LPATT)
      CALL VMCMS('LISTFILE '//CHPATT(1:LPATT)//'(STACK',IRC)
      CALL VMCMS('SENTRIES',NFOUND)
      DO 10 I=1,NFOUND
      CALL VMRTRM(CHFILE,LFILE)
      IF(I.LE.NDONE) GOTO 10
      IF(I.GT.NMAX) THEN
         IRC   = -1
         ICONT = 1
         GOTO 999
      ENDIF
      FILES(I) = CHFILE(1:LFILE)
      NDONE    = NDONE + 1
   10 CONTINUE
#endif
#if defined(CERNLIB_IBMMVS)
*
*     N.B. these routines not yet copied from CSPACK
*
      CALL CDMVSD(CHPATT,CHLIST,LLIST,ICUT,IRC)
 
      IFLAG = 0
      IFILL = 0
      CALL CDLCAT(CHLIST(1:LLIST),FILES,NMAX,NDONE,ICONT)
#endif
  999 END
