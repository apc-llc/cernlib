*
* $Id: fzopen.F,v 1.1.1.1 1996/03/08 15:44:25 mclareni Exp $
*
* $Log: fzopen.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE FZOPEN(LUN,CHFILE,LRECL,CHOPT,IRC)
*
*     Issue FZFILE and perform the open
*     Options are passed to FZFILE, with the addition of 'R' if
*     the file is remote
*
#include "cspack/zmach.inc"
      CHARACTER*(*) CHFILE
      CHARACTER*255 FNAME
      CHARACTER*8   CHOPF,STATE,NODE
      CHARACTER*12  FORMT
      EXTERNAL      FZRMIO
      LOGICAL       IEXIST
#include "cspack/czopts.inc"
*
*     Is the file local or remote
*
      LFILE  = LENOCC(CHFILE)
      LCHOPT = LENOCC(CHOPT)
      ICOLON = INDEX(CHFILE(1:LFILE),'::')
      ISLASH = INDEX(CHFILE(1:LFILE),'//')
*
*     Check if we can access the file directly...
*
      INQUIRE(FILE=CHFILE(1:LFILE),EXIST=IEXIST)
      IF((ICOLON.NE.0).OR.(ISLASH.NE.0) .AND. (.NOT.IEXIST)) THEN
*
*     File is remote (or at least not directly accessible)
*     Issue call to XZ to do the open
*
         LOCAL = 1
         IF(ICOLON.EQ.0) THEN
            IEND  = INDEX(CHFILE(3:LFILE),'/')
            NODE  = CHFILE(1:IEND-1)
            FNAME = CHFILE(IEND+1:LFILE)
         ELSE
            NODE  = CHFILE(1:ICOLON-1)
            FNAME = CHFILE(ICOLON+2:LFILE)
         ENDIF
         CALL XZOPEN(LUN,FNAME,NODE,LRECL,CHOPT,IRC)
      ELSE
*
*     File is local - perform the OPEN
*
         LOCAL  = 0
      FNAME = CHFILE
#if defined(CERNLIB_UNIX)
      CALL CUTOL(FNAME)
#endif
#if defined(CERNLIB_IBM)
*
*     Do not issue OPEN on IBM unless option F (FORTRAN I/O) specified
*
         IF(IOPTF.EQ.0) GOTO 10
#endif
         FORMT = 'UNFORMATTED'
         IF(IOPTA.NE.0) FORMT = 'FORMATTED'
         STATE = 'UNKNOWN'
#if defined(CERNLIB_IBM)
         CALL CTRANS('.',' ',FNAME,1,LFILE)
#endif
#if (!defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_APOLLO))
         OPEN (UNIT=LUN, FORM=FORMT, STATUS=STATE, FILE=FNAME(1:LFILE)
     +      ,ERR=99)
#endif
#if defined(CERNLIB_APOLLO)
         IF(IOPTO.EQ.0) THEN
            OPEN (UNIT=LUN, FORM=FORMT, STATUS=STATE, FILE=FNAME(1:
     +      LFILE), ERR=99)
         ELSE
            OPEN (UNIT=LUN, FORM=FORMT, STATUS=STATE, FILE=FNAME(1:
     +      LFILE), ERR=99)
         ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
         IF(IOPTO.EQ.0) THEN
            OPEN (UNIT=LUN, FORM=FORMT, STATUS=STATE, FILE=FNAME(1:
     +      LFILE), SHARED, READONLY, ERR=99)
         ELSE
            OPEN (UNIT=LUN, FORM=FORMT, STATUS=STATE, FILE=FNAME(1:
     +      LFILE), SHARED, ERR=99)
         ENDIF
#endif
      ENDIF
   10 CONTINUE
      CHOPF = CHOPT(1:LCHOPT)
*     IF(LOCAL.NE.0) CHOPF = CHOPT(1:LCHOPT)//'R'
      IF(LOCAL.NE.0) CHOPF = CHOPT(1:LCHOPT)//'C'
      CALL FZFILE(LUN,LRECL/IQCHAW,CHOPF)
      IF(LOCAL.NE.0) CALL FZHOOK(LUN,FZRMIO,DBUF)
      RETURN
   99 IRC = 1
      END
