*
* $Id: fatrzchk.F,v 1.1.1.1 1996/03/07 15:17:39 mclareni Exp $
*
* $Log: fatrzchk.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:39  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PROGRAM FATRZCHK
*
*     Check that RZ file is not corrupted. Requires that RZVER2
*     is extracted with sequence RZBMAP appropriately defined.
*
*     KDMAX must be >= NRECS * LRECL / 32 (lrecl in words)
*
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
#include "zebra/quest.inc"
*
      COMMON/PAWC/PAW(500000)
      CHARACTER*255 FILE,DEFAULT
      CHARACTER*8   CHWAKE,CHLOGL,CHUSER,CHHOST,CHTYPE,CHSYS,SERNAM
#if defined(CERNLIB_IBMVM)
      CHARACTER*80  LINE
      CHARACTER*1   SERMOD
#endif
 
      INTEGER LENOCC
 
      CALL HLIMIT(500000)
*
*     Get the log level
*
      CALL GETENVF('FMLOGL',CHLOGL)
      IF(IS(1).GT.0) THEN
         IDEBFA = ICDECI(CHLOGL,1,8)
      ELSE
         IDEBFA = 0
      ENDIF
*
*     Get the system name
*
      CALL GETENVF('FATSYS',FILE)
      IF(IS(1).EQ.0) THEN
         FILE = 'CERN'
         IS(1) = 4
      ENDIF
 
      FILE(IS(1)+1:) = '.FATRZ'
      LFILE          = IS(1) + 6
 
      IF(IDEBFA.GE.0) PRINT *,'FATRZCHK. The catalogue name is '//
     +FILE(1:LFILE)
*
*     Get the group
*
      CALL GETENVF('FATGRP',CHUSER)
 
      IF(IS(1).EQ.0) THEN
         CHUSER = 'FMCERN'
         LUSER  = 6
      ELSE
         LUSER  = IS(1)
         CALL CLTOU(CHUSER)
      ENDIF
 
 
      IF(CHUSER(1:2).NE.'FM') THEN
        SERNAM = 'FM' // CHUSER(1:LUSER)
        LUSER  = LUSER + 2
      ELSE
        SERNAM = CHUSER(1:LUSER)
      ENDIF
      LSN    = LUSER
 
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
*
*     Get directory where RZ file is kept...
*
      DEFAULT = ' '
      CALL GETENVF(SERNAM(1:LSN),DEFAULT)
      LDEF = IS(1)
      IF(IDEBFA.GE.0) PRINT *,'FATRZCHK. catalogue is in directory ',
     +   DEFAULT(1:LDEF)
#endif
#if defined(CERNLIB_UNIX)
      LDEF = LDEF + 1
      DEFAULT(LDEF:LDEF) = '/'
#endif
 
      LREC = 0
#if defined(CERNLIB_IBMVM)
*
*     Link to disk of specified service machine
*
      SERMOD = '?'
      CALL VMCMS('EXEC GIME '//SERNAM//
     +'(QUIET NONOTICE STACK)',IRC)
      IF (IRC .LE. 4) THEN
         CALL VMRTRM(LINE,LEN)
         SERMOD = LINE(1:1)
         IF(IDEBFA.GE.0)
     +   PRINT *,'Linked to ',SERNAM,' mode ',SERMOD
      ELSEIF(IRC.EQ.104) THEN
         IF(IDEBFA.GT.-3)
     +   PRINT *,'FATRZCHK. Invalid userid. Check variable FATGRP'
         STOP 16
      ELSE
         IF(IDEBFA.GT.-3)
     +   PRINT *,'FATRZCHK. Error code ',IRC,' from EXEC GIME',
     +           ' type FIND GIME for a list of return codes'
         STOP 16
      ENDIF
      CALL RZOPEN(1,'RZ',FILE(1:LFILE)//' '//SERMOD,' ',LREC,IRC)
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
      CALL RZOPEN(1,'RZ',DEFAULT(1:LDEF)//FILE(1:LFILE),' ',LREC,IRC)
#endif
      IF(ISTAT.NE.0) THEN
         PRINT *,'FATRZCHK. error ,IRC, from RZOPEN'
         GOTO 99
      ENDIF
      CALL RZFILE(1,'RZ',' ')
      CALL RZLOGL(1,1)
      CALL RZVERI('//RZ','OP')
      IRC  = IQUEST(1)
      CALL RZEND('RZ')
      CLOSE(1)
99    CONTINUE
      IF(IRC.NE.0) THEN
         PRINT *,
     +      'FATRZCHK. input RZ file is corrupt or cannot be opened!'
#if !defined(CERNLIB_VAXVMS)
         STOP 16
#endif
#if defined(CERNLIB_VAXVMS)
*%BAS-E-CORFILSTR, Corrupted file structure
         CALL SYS$EXIT(%VAL(1736938))
#endif
      ELSE
         PRINT *,'FATRZCHK. input RZ file seems to be OK!'
      ENDIF
      END
