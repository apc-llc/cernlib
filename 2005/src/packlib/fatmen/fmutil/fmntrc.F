*
* $Id: fmntrc.F,v 1.1.1.1 1996/03/07 15:18:19 mclareni Exp $
*
* $Log: fmntrc.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:19  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_FPACK)||defined(CERNLIB_CSPACK)
      SUBROUTINE FMNTRC(HOST,CHOPT,IRC)
      CHARACTER*(*) HOST
      CHARACTER*20  CHHOST
      CHARACTER*80  LINE
      CHARACTER*20  CHUSER
      CHARACTER*255 CHNAME
      LOGICAL       IEXIST
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
#include "fatmen/fatopts.inc"
*
*     Checks if HOST is in .netrc file
*     The .netrc file is in SYS$LOGIN:NETRC.; (VAX) with IOPTF & IOPTP
*                           SYS$LOGIN:FTPLOGIN.; otherwise
*                           userid.PER.NETRC  (MVS)
*                           $HOME/.netrc      (Unix)
*                           DOT NETRC A0      (VM?)
*
      LHOST  = LENOCC(HOST)
      CHHOST = 'MACHINE '
      CHHOST(9:) = HOST(1:LHOST)
      LHOST  = LHOST + 8
      CALL CLTOU(CHHOST(1:LHOST))
 
      IRC    = 1
      CHNAME = ' '
*
*     Get the name of the netrc file...
*
#if defined(CERNLIB_IBMMVS)
      CALL KPREFI(CHUSER,LUSER)
      CHNAME = '/'//CHUSER(1:LUSER) // '.PER.NETRC'
#endif
#if defined(CERNLIB_IBMVM)
      CHNAME = '/DOT NETRC A0'
#endif
#if defined(CERNLIB_VAXVMS)
      IF(IOPTF.NE.0.AND.IOPTP.NE.0) THEN
         CHNAME = 'SYS$LOGIN:NETRC.;'
      ELSE
         CHNAME = 'SYS$LOGIN:FTPLOGIN.;'
      ENDIF
#endif
#if defined(CERNLIB_UNIX)
      CALL GETENVF('HOME',CHNAME)
      CHNAME(IS(1)+1:IS(1)+7) = '/.netrc'
#endif
      LNAME  = LENOCC(CHNAME)
*
*     Does file exist?
*
      INQUIRE(FILE=CHNAME(1:LNAME),EXIST=IEXIST)
      IF(.NOT.IEXIST) RETURN
*
*     Open the file and look for 'host'
*
      OPEN(LUFZFA,FILE=CHNAME(1:LNAME),FORM='FORMATTED',
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_IBMMVS)
     +     ACTION='READ',
#endif
#if defined(CERNLIB_VAXVMS)
     +     READONLY,
#endif
     +     ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=ISTAT)
      IF(ISTAT.NE.0) RETURN
 
10    CONTINUE
      READ(LUFZFA,'(A)',END=99) LINE
      CALL CLTOU(LINE)
      IF(INDEX(LINE,CHHOST(1:LHOST)) .NE.0) THEN
         IRC = 0
         GOTO 99
      ENDIF
      GOTO 10
 
99    CLOSE(LUFZFA)
 
      END
#endif
