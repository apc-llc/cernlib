*
* $Id: fantrc.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fantrc.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FANTRC(LUN,CHHOST,CHUSER,CHPASS,CHOPT,IRC)
*
*     Check out .netrc file
*
      CHARACTER*(*) CHHOST,CHUSER,CHPASS
      CHARACTER*255 CHFILE,CHBACK
      CHARACTER*20  CHGRP, FMGRP, FMHST, USER
      CHARACTER*80  LINE
      LOGICAL       IEXIST
#if defined(CERNLIB_UNIX)
      INTEGER       SYSTEMF
#endif
#include "fatmen/slate.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatopts.inc"
*
*     The .netrc file is in SYS$LOGIN:FTPLOGIN.; (VMS)
*                           userid.PER.NETRC     (MVS)
*                           $HOME/.netrc         (Unix)
*                           DOT NETRC A0         (VM)
*
 
      LUSER = LENOCC(CHUSER)
      FMGRP = CHUSER(1:LUSER)
      CALL CUTOL(FMGRP(1:LUSER))
 
      LHOST = LENOCC(CHHOST)
      FMHST = CHHOST(1:LHOST)
      CALL CUTOL(FMHST(1:LHOST))
 
#if defined(CERNLIB_IBMMVS)
      CALL KPREFI(USER,LUSER)
      CHFILE = '/'//USER(1:LUSER) // '.PER.NETRC'
#endif
#if defined(CERNLIB_IBMVM)
      CHFILE = '/DOT NETRC A0'
#endif
#if defined(CERNLIB_VAXVMS)
      CHFILE = 'SYS$LOGIN:FTPLOGIN.;'
#endif
#if defined(CERNLIB_UNIX)
      CALL GETENVF('HOME',CHFILE)
      CHFILE(IS(1)+1:IS(1)+7) = '/.netrc'
#endif
      LFILE  = LENOCC(CHFILE)
*
*     Does file exist?
*
      INQUIRE(FILE=CHFILE(1:LFILE),EXIST=IEXIST)
      IF(.NOT.IEXIST) THEN
         PRINT *,'FANTRC. netrc file (',CHFILE(1:LFILE),
     +      ') does not exist'
         IRC = 28
      ENDIF
*
*     Open the file and look for HOST
*
      OPEN(LUN,FILE=CHFILE(1:LFILE),FORM='FORMATTED',
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_IBMMVS)
     +     ACTION='READ',
#endif
#if defined(CERNLIB_VAXVMS)
     +     READONLY,
#endif
     +     ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IRC)
      IF(IRC.NE.0) THEN
         PRINT *,'FANTRC. cannot open netrc file ',CHFILE(1:LFILE)
         RETURN
      ENDIF
*
*     Code for not found
*
      IRC = 4
 
   10 CONTINUE
      READ(LUN,'(A)',END=20) LINE
      CALL CUTOL(LINE)
      IF(INDEX(LINE,FMHST(1:LHOST)).NE.0) THEN
         IRC = 0
         GOTO 30
      ENDIF
      GOTO 10
 
   20 CONTINUE
      CLOSE(LUN)
      PRINT *,'FANTRC. .netrc file not setup for use with ', FMHST(1:
     +LHOST)
      GOTO 40
   30 CONTINUE
*
*     Found a 'host' line
*
      LLINE = LENOCC(LINE)
      CALL CSQMBL(LINE,1,LLINE)
      LLINE = IS(1)
#if defined(CERNLIB_VAXVMS)
*
*     Syntax: host user password
*
      LBLANK = INDEX(LINE,' ')
      JBLANK = INDEXB(LINE(1:LLINE),' ')
      CHGRP = LINE(LBLANK+1:JBLANK-1)
      CHPASS = LINE(JBLANK+1:LLINE)
      LGRP = JBLANK - LBLANK - 1
#endif
#if !defined(CERNLIB_VAXVMS)
*
*     Syntax: machine host login username password password
*
      LBLANK = INDEX(LINE,' login ') + 7
      KBLANK = INDEX(LINE,' password ') -1
      JBLANK = INDEXB(LINE(1:LLINE),' ')
      CHGRP = LINE(LBLANK:KBLANK)
      CHPASS = LINE(JBLANK+1:LLINE)
      LGRP = KBLANK - LBLANK + 1
#endif
 
   40 CONTINUE
*
*     Check if user name matches
*
      IF(FMGRP(1:LUSER).NE.CHGRP(1:LGRP)) THEN
         IRC = -1
         IF(IDEBFA.GE.-3) PRINT *,'FANTRC. username in netrc file ('//
     +      CHGRP(1:LGRP)//') does not match the FATMEN group ('//
     +      FMGRP(1:LUSER)//')'
      ENDIF
 
      END
 
