*
* $Id: xzlls.F,v 1.1.1.1 1996/03/08 15:44:30 mclareni Exp $
*
* $Log: xzlls.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZLLS(CHPATT,FILES,NMAX,NFOUND,ICONT,CHOPT,IRC)
#include "cspack/czsock.inc"
#include "cspack/czunit.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
      CHARACTER*(*) FILES(NMAX)
      CHARACTER*(*) CHPATT
      CHARACTER*255 CHLIST
      CHARACTER*255 CHFILE,CHDEF
      CHARACTER*255 COMM
      CHARACTER*8   CHRAND
      CHARACTER*2   CHOPTT
      CHARACTER*255 CHLOG,CHERR
      CHARACTER*255 CHHOME
      COMMON/SLATE/IS(40)
      INTEGER       SYSTEMF
      LOGICAL       IEXIST
      SAVE          NDONE
*
#if defined(CERNLIB_VAXVMS)
      INCLUDE '($RMSDEF)'
      INTEGER SYS$GETMSG
      CHARACTER*255 CHDIR
#endif
#include "cspack/czopts.inc"
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
         IF(INDEX(CHPATT(1:LPATT),'.').EQ.0) THEN
            CHPATT(LPATT+1:LPATT+2) = '.*'
            LPATT = LPATT + 2
         ENDIF
#endif
      ENDIF
#if defined(CERNLIB_UNIX)
      CALL CUTOL(CHLIST)
#endif
      IF(IDEBXZ.GE.1) PRINT *,'XZLLS. enter for path = ',
     +  CHLIST(1:LPATT)
      NERR   = 0
      NFOUND = 0
      CHOPTT = '  '
      COMM   = ' '
      IF(IOPTL.NE.0) CHOPTT = '-l'
#if defined(CERNLIB_UNIX)
      CALL GETENVF('HOME',CHHOME)
      LHOME = IS(1)
      CALL CZRAND(CHRAND)
      CHLOG = CHHOME(1:LHOME)//'/'//CHRAND//'zftp.tmp'
      CHERR = CHHOME(1:LHOME)//'/'//CHRAND//'zftp.err'
      LLOG  = LENOCC(CHLOG)
      LERR  = LLOG
      INQUIRE(FILE=CHLOG(1:LLOG),EXIST=IEXIST)
      IF(.NOT.IEXIST.OR.ICONT.EQ.0) THEN
         WRITE(COMM,9001) CHOPTT,CHLIST(1:LPATT),
     +                    CHLOG(1:LLOG),CHERR(1:LERR)
 9001 FORMAT('ls ',A,' ',A,' >',A,' 2>',A)
         IC = SYSTEMF(COMM)
         NDONE = 0
      ENDIF
 
      OPEN(LUNXZI,FILE=CHLOG(1:LLOG),FORM='FORMATTED',
     +     STATUS='OLD',IOSTAT=ISTAT)
      IF(ISTAT.NE.0) GOTO 99
      ISKIP = 0
10    CONTINUE
      READ(LUNXZI,'(A)',END=98) CHFILE
      ISKIP = ISKIP + 1
      IF(ISKIP.LT.NDONE) THEN
         GOTO 10
      ENDIF
      NFOUND = NFOUND + 1
      IF(NFOUND.GT.NMAX) THEN
         NFOUND = NMAX
         IRC    = -1
         ICONT  = 1
         GOTO 98
      ENDIF
      NDONE = NDONE + 1
      FILES(NFOUND) = CHFILE(1:LENOCC(CHFILE))
      GOTO 10
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_APOLLO))
98    CLOSE(LUNXZI,STATUS='DELETE')
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_APOLLO))
98    CLOSE(LUNXZI)
#endif
#if defined(CERNLIB_UNIX)
      IF(NFOUND.EQ.0) THEN
         IRC = 1
         OPEN(LUNXZI,FILE=CHERR(1:LERR),FORM='FORMATTED',
     +        STATUS='OLD')
11       CONTINUE
         READ(LUNXZI,'(A)',END=97) CHFILE
         NFOUND = NFOUND + 1
         NERR   = NERR + 1
         FILES(NFOUND) = CHFILE(1:LENOCC(CHFILE))
         GOTO 11
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_APOLLO))
97    CLOSE(LUNXZI,STATUS='DELETE')
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_APOLLO))
97    CLOSE(LUNXZI)
#endif
#if defined(CERNLIB_UNIX)
         IF(NERR.EQ.0) IRC = 0
      ELSE
*
*     Remove error file
*
         IC = UNLINKF(CHERR(1:LERR))
      ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
      CHDEF  = ' '
      CALL GETDEF(CHDEF)
      LDEF = LENOCC(CHDEF)
10    CONTINUE
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
         CALL LIB$FIND_FILE_END(ICONT)
         IRC = ISTAT
         GOTO 99
      ELSEIF(ISTAT.EQ.RMS$_FNF) THEN
         CALL LIB$FIND_FILE_END(ICONT)
         GOTO 99
      ELSEIF(ISTAT.EQ.RMS$_NMF) THEN
         CALL LIB$FIND_FILE_END(ICONT)
         GOTO 99
      ENDIF
 
      IF(.NOT.ISTAT) THEN
         IRC = SYS$GETMSG(%VAL(ISTAT),LFILE,CHFILE,,)
         PRINT *,'XZLLS. error from LIB$FIND_FILE = ',CHFILE(1:LFILE)
         IRC = ISTAT
         ISTAT = LIB$FIND_FILE_END(ICONT)
         GOTO 99
      ELSE
         NFOUND = NFOUND + 1
         IF(NFOUND.GT.NMAX) GOTO 99
         FILES(NFOUND) = CHFILE(1:LENOCC(CHFILE))
         GOTO 10
      ENDIF
 
#endif
#if defined(CERNLIB_IBMVM)
      IF(ICONT.EQ.0) NDONE = 0
      CALL CTRANS('.',' ',CHPATT,1,LPATT)
      IF(IOPTL.EQ.0) THEN
         CALL VMCMS('LISTFILE '//CHPATT(1:LPATT)//'(STACK',IRC)
      ELSE
         CALL VMCMS('LISTFILE '//CHPATT(1:LPATT)//'(STACK L',IRC)
      ENDIF
      CALL VMCMS('SENTRIES',NFOUND)
      DO 10 I=1,NFOUND
      CALL VMRTRM(CHFILE,LFILE)
      IF(I.LE.NDONE) GOTO 10
      IF(I.GT.NMAX) THEN
         IRC   = -1
         ICONT = 1
         GOTO 99
      ENDIF
      FILES(I) = CHFILE(1:LFILE)
      NDONE    = NDONE + 1
10    CONTINUE
#endif
#if defined(CERNLIB_IBMMVS)
      CALL XZMVSD(CHPATT,CHLIST,LLIST,ICUT,IRC)
 
      IFLAG = 0
      IFILL = 0
1     CONTINUE
      CALL XZLCAT(CHLIST(1:LLIST),FILES,NMAX,NDONE,ICONT)
#endif
99    END
