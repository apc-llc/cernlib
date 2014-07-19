*
* $Id: szls.F,v 1.1.1.1 1996/03/08 15:44:27 mclareni Exp $
*
* $Log: szls.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:27  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SZLS(CHPATT,CHOPT,IRC)
      CHARACTER*(*) CHPATT
      CHARACTER*255 CHLIST,CHLOG,CHERR
      CHARACTER*255 CHFILE,CHDEF
      CHARACTER*255 COMM
      CHARACTER*8   CHRAND
      CHARACTER*2   CHOPTT
      INTEGER       SYSTEMF
#include "cspack/czunit.inc"
#if defined(CERNLIB_IBMMVS)
      PARAMETER     (MAXDSN=10)
      CHARACTER*80  DSN(MAXDSN)
#endif
#include "cspack/czhome.inc"
#include "cspack/czsock.inc"
#if defined(CERNLIB_UNIX)
      CHARACTER*8 LS
#endif
#if defined(CERNLIB_VAXVMS)
#include "cspack/vmsinf.inc"
      CHARACTER*10  CBLOCKS
      INCLUDE       '($RMSDEF)'
      INTEGER       SYS$GETMSG
      CHARACTER*255 CHDIR
      EXTERNAL      XZFINF
#endif
#include "cspack/czopts.inc"
*
*     Return files matching pattern CHPATT
*
      LPATT  = LENOCC(CHPATT)
      IF(LPATT.EQ.0) THEN
#if defined(CERNLIB_IBM)
         LPATT  = 1
         CHLIST = '*'
#endif
#if defined(CERNLIB_UNIX)
         LPATT  = 1
         CHLIST = ' '
#endif
#if defined(CERNLIB_IBM)||defined(CERNLIB_UNIX)
      ELSE
         CHLIST = CHPATT(1:LPATT)
#endif
#if defined(CERNLIB_VAXVMS)
         LPATT  = 3
         CHPATT = '*.*'
      ELSE
         IF((CHPATT(LPATT:LPATT).EQ.':').OR.
     +      (CHPATT(LPATT:LPATT).EQ.'>').OR.
     +      (CHPATT(LPATT:LPATT).EQ.']')) THEN
             CHPATT(LPATT+1:LPATT+3) = '*.*'
             LPATT = LPATT + 3
         ENDIF
         IF(INDEX(CHPATT(1:LPATT),'.').EQ.0) THEN
            CHPATT(LPATT+1:LPATT+2) = '.*'
            LPATT = LPATT + 2
         ENDIF
#endif
      ENDIF
      NERR   = 0
      NFOUND = 0
      ICONT  = 0
      CHOPTT = '  '
      COMM   = ' '
      IF(IOPTL.NE.0) CHOPTT = '-l'
      IF(IOPTD.NE.0) CHOPTT = '-d'
#if defined(CERNLIB_UNIX)
      CALL CZRAND(CHRAND)
      CHLOG = CHHOME(1:LHOME)//'/'//CHRAND//'zftp.tmp'
      CHERR = CHHOME(1:LHOME)//'/'//CHRAND//'zftp.err'
      LLOG  = LENOCC(CHLOG)
      LERR  = LLOG
      WRITE(COMM,9001) CHOPTT,CHLIST(1:LPATT),
     +                 CHLOG(1:LLOG),CHERR(1:LERR)
 9001 FORMAT('ls ',A,' ',A,' >',A,' 2>',A)
      IC = SYSTEMF(COMM)
      OPEN(LUNXZI,FILE=CHLOG(1:LLOG),FORM='FORMATTED',
     +     STATUS='OLD')
10    CONTINUE
      READ(LUNXZI,'(A)',END=98) CHFILE
      CALL CZPUTA('2'//CHFILE(1:LENOCC(CHFILE)),IRC)
      NFOUND = NFOUND + 1
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
         NERR = NERR + 1
         CALL CZPUTA('2'//CHFILE(1:LENOCC(CHFILE)),IRC)
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
10    CONTINUE
      CHFILE = ' '
      CHDEF  = ' '
      CALL GETDEF(CHDEF)
      LDEF = LENOCC(CHDEF)
      IF ((LDEF.NE.0) .AND.
     +   (INDEX(CHPATT(1:LPATT),':').EQ.0).AND.
     +   (INDEX(CHPATT(1:LPATT),'>').EQ.0).AND.
     +   (INDEX(CHPATT(1:LPATT),'-').EQ.0)) THEN
         ISTAT = LIB$FIND_FILE(CHDEF(1:LDEF)//
     +                         CHPATT(1:LPATT),CHFILE,ICONT)
      ELSE
         ISTAT = LIB$FIND_FILE(CHPATT(1:LPATT),CHFILE,ICONT)
      ENDIF
*
*     Check for RMS$_DNF - directory not found
*               RMS$_FNF - file      not found
*               RMS$_NMF - no more files
*
      IF(ISTAT.EQ.RMS$_NMF) THEN
         ISTAT = LIB$FIND_FILE_END(ICONT)
         GOTO 99
      ENDIF
 
      IF(.NOT.ISTAT) THEN
         IRC = SYS$GETMSG(%VAL(ISTAT),LFILE,CHFILE,,)
         CALL CZPUTA('2'//CHFILE(1:LFILE),IRC)
         IRC = ISTAT
         ISTAT = LIB$FIND_FILE_END(ICONT)
         GOTO 99
      ELSE
*
*     Get recfm, nblocks, creation date, if IOPTL
*
         LF = LENOCC(CHFILE)
         IF(IOPTL.NE.0) THEN
            OPEN(99,FILE=CHFILE(1:LF),STATUS='OLD',READONLY,
     +           IOSTAT=IRC,USEROPEN=XZFINF)
            ISTART = INDEXB(CHFILE(1:LF),']')+1
            IF(NFOUND.EQ.0) THEN
               CALL CZPUTA('2'//'Directory: '//CHFILE(1:ISTART-1),IRC)
               CALL CZPUTA('2',IRC)
            ENDIF
            WRITE(CBLOCKS,'(I10)') IBLK
            CALL CZPUTA('2'//CHFILE(ISTART:LF)//' '//CBLOCKS//' '//
     +         CREDAT(1:17)//' '//
     +         CHUIC(1:LENOCC(CHUIC))//' '//CHPROT,IRC)
         ELSE
            CALL CZPUTA('2'//CHFILE(1:LF),IRC)
         ENDIF
         NFOUND = NFOUND + 1
         GOTO 10
      ENDIF
 
#endif
#if defined(CERNLIB_IBMMVS)
      CALL XZMVSD(CHPATT,CHLIST,LLIST,ICUT,IRC)
* added Thomas Schwab  6/12/91
*-----------------------------------
      LPATT = LLIST
 
      IFLAG = 0
      IFILL = 0
1     CONTINUE
      CALL XZLCAT(CHLIST(2:LPATT),DSN,MAXDSN,IFILL,IFLAG)
      DO 2 I=1,MIN(MAXDSN,IFILL)
         CALL CZPUTA('2'//DSN(I)((ICUT):LENOCC(DSN(I))),IRC)
2     CONTINUE
      IF(IFLAG.NE.0) GOTO 1
#endif
#if defined(CERNLIB_IBMVM)
      CALL CTRANS('.',' ',CHPATT,1,LPATT)
      IF(IOPTL.EQ.0) THEN
         CALL VMCMS('LISTFILE '//CHPATT(1:LPATT)//'(STACK',IRC)
         ELSE
         CALL VMCMS('LISTFILE '//CHPATT(1:LPATT)//'(STACK L',IRC)
         ENDIF
      CALL VMCMS('SENTRIES',NFOUND)
      DO 10 I=1,NFOUND
      CALL VMRTRM(CHFILE,LFILE)
      CALL CZPUTA('2'//CHFILE(1:LENOCC(CHFILE)),IRC)
10    CONTINUE
#endif
99    END
