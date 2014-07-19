*
* $Id: xzlmv.F,v 1.1.1.1 1996/03/08 15:44:30 mclareni Exp $
*
* $Log: xzlmv.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZLMV(LOCAL,REMOTE,CHOPT,IRC)
      CHARACTER*(*) LOCAL,REMOTE
      CHARACTER*255 CHSRC,CHDST
#include "cspack/czunit.inc"
#if defined(CERNLIB_VAXVMS)
      CHARACTER*255 CHMESS
#endif
#if defined(CERNLIB_UNIX)
      CHARACTER*255 CHCOMM,CHLOG,CHERR
      INTEGER       SYSTEMF
#include "cspack/slate.inc"
#include "cspack/czhome.inc"
#endif
#include "cspack/czopts.inc"
 
      LSRC = LENOCC(LOCAL)
      LDST = LENOCC(REMOTE)
      CHSRC = LOCAL(1:LSRC)
      CHDST = REMOTE(1:LDST)
#if defined(CERNLIB_IBMVM)
      CALL CTRANS('.',' ',CHSRC,1,LSRC)
      CALL CTRANS('.',' ',CHDST,1,LDST)
      CALL VMCMS('RENAME '//CHSRC(1:LSRC)//' '//CHDST(1:LDST),IRC)
#endif
#if defined(CERNLIB_VAXVMS)
      ISTAT  = LIB$RENAME_FILE(CHSRC(1:LSRC),CHDST(1:LDST),,,,,,,,,,)
      IF(.NOT.ISTAT) THEN
         IC  = SYS$GETMSG(%VAL(ISTAT),LMESS,CHMESS,,)
         PRINT *,'XZLMV. error from LIB$RENAME_FILE = ',
     +           CHMESS(1:LMESS)
         IRC = ISTAT
      ELSE
         IRC = 0
      ENDIF
#endif
#if defined(CERNLIB_UNIX)
      IF(IOPTC.EQ.0) THEN
         CALL CUTOL(CHSRC(1:LSRC))
         CALL CUTOL(CHDST(1:LDST))
      ENDIF
      CALL GETENVF('HOME',CHHOME)
      LHOME = IS(1)
      CHLOG = CHHOME(1:LHOME)//'/zftp.tmp'
      CHERR = CHHOME(1:LHOME)//'/zftp.err'
      LLOG  = LENOCC(CHLOG)
      LERR  = LLOG
      WRITE(CHCOMM,9001) CHSRC(1:LSRC),CHDST(1:LDST),
     +                 CHLOG(1:LLOG),CHERR(1:LERR)
 9001 FORMAT('mv ',A,' ',A,' >',A,' 2>',A)
      IC = SYSTEMF(CHCOMM)
      OPEN(LUNXZI,FILE=CHLOG(1:LLOG),FORM='FORMATTED',
     +     STATUS='OLD')
   10 CONTINUE
      READ(LUNXZI,'(A)',END=20) CHCOMM
      PRINT *,'XZLMV. error from mv = ',
     +        CHCOMM(1:LENOCC(CHCOMM))
      GOTO 10
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_APOLLO))
   20 CLOSE(LUNXZI,STATUS='DELETE')
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_APOLLO))
   20 CLOSE(LUNXZI)
#endif
#if defined(CERNLIB_UNIX)
      NERR = 0
      OPEN(LUNXZI,FILE=CHERR(1:LERR),FORM='FORMATTED',
     +     STATUS='OLD')
   30 CONTINUE
      READ(LUNXZI,'(A)',END=40) CHCOMM
      NERR = NERR + 1
      PRINT *,'XZLMV. error from mv = ',
     +        CHCOMM(1:LENOCC(CHCOMM))
      GOTO 30
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_APOLLO))
   40 CLOSE(LUNXZI,STATUS='DELETE')
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_APOLLO))
   40 CLOSE(LUNXZI)
      IRC = NERR
#endif
      END
