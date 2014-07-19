*
* $Id: pzntrc.F,v 1.1.1.1 1996/03/08 15:44:22 mclareni Exp $
*
* $Log: pzntrc.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:22  mclareni
* Cspack
*
*
#include "sys/CERNLIB_machine.h"
#include "_cspack/pilot.h"
      SUBROUTINE PZNTRC(LUN,CHGRP,CHPASS,CHOPT,IRC)
*
*     Check out .netrc file
*
      CHARACTER*(*) CHGRP,CHPASS
      CHARACTER*255 CHFILE,CHBACK
      CHARACTER*20  CHUSER
      CHARACTER*80  LINE
      LOGICAL       IEXIST
#if defined(CERNLIB_VAXVMS)
*     INTEGER       XZPROT
*     EXTERNAL      XZPROT
      INTEGER       SYS$SETDFPROT
#endif
#if defined(CERNLIB_UNIX)
      INTEGER       SYSTEMF
#endif
#include "slate.inc"
#include "pzunit.inc"
#if defined(CERNLIB_VAXVMS)
      DATA          IPROT/Z0000FFCF/
#endif
*
*     IRC =  4 : entry for 'piaf' does not exist
*           28 : netrc file not found
*
#include "czopts.inc"
*
*     The .netrc file is in SYS$LOGIN:FTPLOGIN.; (VMS)
*                           userid.PER.NETRC     (MVS)
*                           $HOME/.netrc         (Unix)
*                           DOT NETRC A0         (VM)
*
*     Options: C - check if .netrc file exists, contains
*                  a line 'machine piaf user ...'
*              R - remove piaf line as password check fails
*              U - update .netrc file
*
#if defined(CERNLIB_IBMMVS)
      CALL KPREFI(CHUSER)
      CHFILE = '/'//CHUSER(1:LENOCC(CHUSER)) // '.PER.NETRC'
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
      IF(.NOT.IEXIST.AND.IOPTC.NE.0) THEN
         PRINT *,'PZNTRC. netrc file (',CHFILE(1:LFILE),
     +      ') does not exist'
         PRINT *,'PZNTRC. file will be created by'//
     +      ' the CONNECT command'
         IRC = 28
         RETURN
      ENDIF
 
      IF(IOPTC.NE.0) THEN
*
*     Open the file and look for 'piaf'
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
            PRINT *,'PZNTRC. cannot open netrc file ',CHFILE(1:LFILE)
            RETURN
         ENDIF
*
*     Not found
*
         IRC = 4
 
   10    CONTINUE
         READ(LUN,'(A)',END=20) LINE
         IF(INDEX(LINE,'piaf1').NE.0) THEN
            IRC = 0
            GOTO 30
         ENDIF
         GOTO 10
 
   20    CONTINUE
         CLOSE(LUN)
         PRINT *,'PZNTRC. .netrc file not setup for use with PIAF'
         PRINT *,'PZNTRC. use the CONNECT command to rectify this'
         GOTO 40
   30    CONTINUE
         CLOSE(LUN)
*
*     Found a 'piaf' line
*
         LLINE = LENOCC(LINE)
         CALL CSQMBL(LINE,1,LLINE)
         LLINE = IS(1)
#if defined(CERNLIB_VAXVMS)
*
*     Syntax: piaf user password
*
         LBLANK = INDEX(LINE,' ')
         JBLANK = INDEXB(LINE(1:LLINE),' ')
         CHGRP  = LINE(LBLANK+1:JBLANK-1)
         CHPASS = LINE(JBLANK+1:LLINE)
         LGRP   = JBLANK - LBLANK - 1
#endif
#if !defined(CERNLIB_VAXVMS)
*
*     Syntax: machine piaf login username password password
*
         LBLANK = INDEX(LINE,' login ') + 7
         KBLANK = INDEX(LINE,' password ') -1
         JBLANK = INDEXB(LINE(1:LLINE),' ')
         CHGRP  = LINE(LBLANK:KBLANK)
         CHPASS = LINE(JBLANK+1:LLINE)
         LGRP   = KBLANK - LBLANK + 1
#endif
 
         PRINT *,'PZNTRC. .netrc setup for group ',CHGRP(1:LGRP)
         PRINT *,'PZNTRC. use the CONNECT command to change group ',
     +   'and/or password'
 
   40    CONTINUE
      ENDIF
*
*     Update / create file
*
      IF(IOPTR.NE.0.OR.IOPTU.NE.0) THEN
 
         IF(IEXIST) THEN
*
*     Rename existing file to .bak
*
#if defined(CERNLIB_IBMVM)
            CHBACK = CHFILE(1:LFILE) // 'BAK'
            LBACK = LFILE + 3
            CALL VMCMS('RENAME '//CHFILE(2:LFILE)// ' '//CHBACK(2:
     +      LBACK),IRC)
            OPEN(LUNPZO,FILE=CHBACK(1:LBACK),STATUS='OLD', ACTION=
     +      'READ', FORM='FORMATTED',ACCESS='SEQUENTIAL',IOSTAT=ISTAT)
            OPEN(LUNPZN,FILE=CHFILE(1:LFILE),STATUS='NEW', ACTION=
     +      'WRITE', FORM='FORMATTED',ACCESS='SEQUENTIAL',IOSTAT=ISTAT)
#endif
#if defined(CERNLIB_VAXVMS)
            CHBACK = CHFILE(1:LFILE-1) // 'BAK'
            LBACK = LFILE + 2
            ISTAT = LIB$RENAME_FILE(CHFILE(1:LFILE), CHBACK(1:LBACK),,,
     +      ,,,,,,,)
            IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
            OPEN(LUNPZO,FILE=CHBACK(1:LBACK),STATUS='OLD', READONLY,
     +      SHARED, FORM='FORMATTED',ACCESS='SEQUENTIAL',IOSTAT=ISTAT)
            IC = SYS$SETDFPROT(IPROT,JPROT)
            OPEN(LUNPZN,FILE=CHFILE(1:LFILE),STATUS='NEW',
     +      CARRIAGECONTROL='LIST',FORM='FORMATTED',
     +      ACCESS='SEQUENTIAL',IOSTAT=ISTAT)
            IC = SYS$SETDFPROT(JPROT,%VAL(0))
#endif
#if defined(CERNLIB_UNIX)
            CHBACK = CHFILE(1:LFILE) // '.bak'
            LBACK = LFILE + 4
            INQUIRE(FILE=CHBACK(1:LBACK),EXIST=IEXIST)
            IF(IEXIST) THEN
               ISTAT = SYSTEMF('chmod +w '// CHBACK(1:LBACK))
            ENDIF
            ISTAT = SYSTEMF('mv '//CHFILE(1:LFILE)//' '// CHBACK(1:
     +      LBACK))
            OPEN(LUNPZO,FILE=CHBACK(1:LBACK),STATUS='OLD', FORM=
     +      'FORMATTED',ACCESS='SEQUENTIAL',IOSTAT=ISTAT)
            OPEN(LUNPZN,FILE=CHFILE(1:LFILE),STATUS='NEW', FORM=
     +      'FORMATTED',ACCESS='SEQUENTIAL',IOSTAT=ISTAT)
#endif
*
*     Now copy all records from the old file, except those that
*     mention 'piaf'
*
   60       CONTINUE
            READ(LUNPZO,'(A)',END=70) LINE
            LLINE = LENOCC(LINE)
#if defined(CERNLIB_VAXVMS)
            IF(INDEX(LINE(1:LLINE),'piaf1 ').NE.0) GOTO 60
#endif
#if defined(CERNLIB_VAXVMS)
            IF(INDEX(LINE(1:LLINE),'machine piaf1').NE.0) GOTO 60
#endif
            WRITE(LUNPZN,'(A)') LINE(1:LLINE)
            GOTO 60
   70       CONTINUE
            CLOSE(LUNPZO)
            IF(IOPTR.EQ.0) THEN
               LGRP = LENOCC(CHGRP)
               LPASS = LENOCC(CHPASS)
               WRITE(LUNPZN,9001) CHGRP(1:LGRP),CHPASS(1:LPASS)
#if defined(CERNLIB_VAXVMS)
 9001       FORMAT('piaf1 ',A,1X,A)
#endif
#if !defined(CERNLIB_VAXVMS)
 9001       FORMAT('machine piaf1 login ',A,' password ',A)
#endif
            ENDIF
            CLOSE(LUNPZN)
 
         ELSE
            LGRP = LENOCC(CHGRP)
            LPASS = LENOCC(CHPASS)
#if defined(CERNLIB_IBMVM)
            OPEN(LUNPZN,FILE=CHFILE(1:LFILE),STATUS='NEW', ACTION=
     +      'WRITE', FORM='FORMATTED',ACCESS='SEQUENTIAL',IOSTAT=ISTAT)
#endif
#if defined(CERNLIB_UNIX)
            OPEN(LUNPZN,FILE=CHFILE(1:LFILE),STATUS='NEW', FORM=
     +      'FORMATTED',ACCESS='SEQUENTIAL',IOSTAT=ISTAT)
#endif
#if defined(CERNLIB_VAXVMS)
            IC = SYS$SETDFPROT(IPROT,JPROT)
            OPEN(LUNPZN,FILE=CHFILE(1:LFILE),STATUS='NEW',
     +      CARRIAGECONTROL='LIST',FORM='FORMATTED',
     +      ACCESS='SEQUENTIAL',IOSTAT=ISTAT)
            IC = SYS$SETDFPROT(JPROT,%VAL(0))
#endif
            WRITE(LUNPZN,9001) CHGRP(1:LGRP),CHPASS(1:LPASS)
            CLOSE(LUNPZN)
         ENDIF
      ENDIF
 
#if defined(CERNLIB_UNIX)
      ISTAT = SYSTEMF('chmod 400 '//CHFILE(1:LFILE))
#endif
      END
