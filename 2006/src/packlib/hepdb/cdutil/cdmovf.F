*
* $Id: cdmovf.F,v 1.1.1.1 1996/02/28 16:24:44 mclareni Exp $
*
* $Log: cdmovf.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:44  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDMOVF(CHOLD,CHNEW,CHOPT,IRC)
      CHARACTER*(*) CHOLD,CHNEW
      CHARACTER*80  CHLINE,CHTYPE,CHDEV,CHLOG
#include "hepdb/hdbopts.inc"
 
      IRC = 0
 
* Move file from CHOLD to CHNEW.
* If IOPTK = 0 then CHOLD will be deleted
 
#if defined(CERNLIB_VAXVMS)
 
      ISTAT = LIB$GET_LUN(LUNIN)
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      ISTAT = LIB$GET_LUN(LUNOU)
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
 
      LOLD = LENOCC(CHOLD)
      LNEW = LENOCC(CHNEW)
 
      CHTYPE = 'VARIABLE'
      ICOLON = INDEX(CHNEW(1:LNEW),':')
      IF(ICOLON.NE.0) THEN
         CHDEV = CHNEW(1:ICOLON-1)
         CALL CLTOU(CHDEV(1:ICOLON-1))
         CALL CDGTLG(CHDEV,CHLOG,'LNM$SYSTEM',IRC)
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_UCX))
         IF(IRC.EQ.0.AND.INDEX(CHLOG,'DNFS').NE.0)
     +      CHTYPE='STREAM_LF'
#endif
#if (defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_UCX))
         IF(IRC.EQ.0.AND.INDEX(CHLOG,'_NFS').NE.0)
     +      CHTYPE='STREAM_LF'
#endif
#if defined(CERNLIB_VAXVMS)
 
      ENDIF
 
      OPEN(LUNIN,ACCESS='SEQUENTIAL',STATUS='OLD',
     +     FILE=CHOLD(1:LOLD),
     +     FORM='FORMATTED',IOSTAT=IRC)
      IF(IRC.NE.0) GOTO 99
 
      OPEN(LUNOU,ACCESS='SEQUENTIAL',STATUS='NEW',
     +     FILE=CHNEW(1:LNEW),
     +     RECORDTYPE=CHTYPE,
     +     FORM='FORMATTED',IOSTAT=IRC)
      IF(IRC.NE.0) GOTO 99
 
10    CONTINUE
 
      READ(LUNIN,'(A)',END=90) CHLINE
      LLINE = LENOCC(CHLINE)
      WRITE(LUNOU,'(A)') CHLINE(1:LLINE)
      GOTO 10
 
90    CONTINUE
      IF(IOPTK.EQ.0) THEN
         CLOSE(LUNIN,STATUS='DELETE')
      ELSE
         CLOSE(LUNIN)
      ENDIF
      CLOSE(LUNOU)
 
      ISTAT = LIB$FREE_LUN(LUNIN)
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      ISTAT = LIB$FREE_LUN(LUNOU)
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
#endif
99    CONTINUE
      END
