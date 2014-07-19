*
* $Id: szmv.F,v 1.1.1.1 1996/03/08 15:44:27 mclareni Exp $
*
* $Log: szmv.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:27  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SZMV(LOCAL,REMOTE,CHOPT,IRC)
      CHARACTER*(*) LOCAL,REMOTE
      CHARACTER*255 CHSRC,CHDST
#include "cspack/czunit.inc"
#if defined(CERNLIB_VAXVMS)
      CHARACTER*255 CHMESS
#endif
#if defined(CERNLIB_UNIX)
      CHARACTER*255 CHCOMM,CHLOG,CHERR
      INTEGER       SYSTEMF
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
         CALL CZPUTA('2'//CHMESS(1:LMESS),IRC)
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
      READ(LUNXZI,'(A)',END=98) CHCOMM
      CALL CZPUTA('2'//CHCOMM(1:LENOCC(CHCOMM)),IRC)
      GOTO 10
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_APOLLO))
   98 CLOSE(LUNXZI,STATUS='DELETE')
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_APOLLO))
   98 CLOSE(LUNXZI)
#endif
#if defined(CERNLIB_UNIX)
      NERR = 0
      OPEN(LUNXZI,FILE=CHERR(1:LERR),FORM='FORMATTED',
     +     STATUS='OLD')
   11 CONTINUE
      READ(LUNXZI,'(A)',END=97) CHCOMM
      NERR = NERR + 1
      CALL CZPUTA('2'//CHCOMM(1:LENOCC(CHCOMM)),IRC)
      GOTO 11
   97 CONTINUE
      IRC = NERR
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_APOLLO))
      CLOSE(LUNXZI,STATUS='DELETE')
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_APOLLO))
      CLOSE(LUNXZI)
#endif
 
      END
