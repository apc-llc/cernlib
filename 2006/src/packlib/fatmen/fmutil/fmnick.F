*
* $Id: fmnick.F,v 1.1.1.1 1996/03/07 15:18:19 mclareni Exp $
*
* $Log: fmnick.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:19  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMNICK(LUN,CHFILE,CHNICK,CHNAME,CHDESC,CHOPT,IRC)
*
*     Search in NAMES file 'CHFILE' for generic name and description
*     corresponding to nickname 'CHNICK'
*
      CHARACTER*(*) CHFILE,CHNICK,CHNAME,CHDESC
      CHARACTER*255 CHIN(2,1),CHOUT(2,2)
      CHARACTER*255 CHRNGE,CHFNAM
      LOGICAL       IEXIST
#include "fatmen/fatbug.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatopts.inc"
 
      IRC = 0
 
      CHNAME = ' '
      CHDESC = ' '
 
      LFILE  = LENOCC(CHFILE)
      JFILE  = LFILE
 
      IF(LFILE.EQ.0) THEN
         LDEF = LENOCC(DEFAULT)
         LSN  = LENOCC(SERNAM)
#if defined(CERNLIB_CSPACK)
         IF(FATNOD.NE.' ') THEN
            CHFNAM = DEFAULT(1:LDEF)//'/'//SERNAM(1:LSN)//'.NAMES'
            LFILE = LENOCC(CHFNAM)
            CALL CUTOL(CHFNAM(1:LFILE))
            IF(IDEBFA.GE.0) PRINT *,'FMNICK. using default names file -'
     +      //' ', CHFNAM(1:LFILE),' on node ',FATNOD
            CALL XZINQR(LUFZFA,CHFNAM(1:LFILE),FATNOD,LRECL,ICODE,IRC)
            IF(ICODE.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMNICK. names file ', 'does '
     +         //'not exist (',CHFNAM(1:LFILE),')'
               IRC = 28
               RETURN
            ENDIF
 
         ELSE
#endif
#if defined(CERNLIB_UNIX)
            CHFNAM = DEFAULT(1:LDEF)//'/'//SERNAM(1:LSN)//'.NAMES'
#endif
#if defined(CERNLIB_VAXVMS)
            CHFNAM = DEFAULT(1:LDEF)//SERNAM(1:LSN)//'.NAMES'
#endif
#if defined(CERNLIB_IBMVM)
            CHFNAM = '/'//SERNAM(1:LSN)//' NAMES '//SERMOD
#endif
#if defined(CERNLIB_IBMMVS)
            CHFNAM = '/'//DEFAULT(1:LDEF)//'.'//SERNAM(1:LSN)//
     +      '.NAMES'
#endif
            LFILE = LENOCC(CHFNAM)
#if defined(CERNLIB_UNIX)
            CALL CUTOL(CHFNAM(1:LFILE))
#endif
            IF(IDEBFA.GE.0) PRINT *,'FMNICK. using default names file -'
     +      //' ', CHFNAM(1:LFILE)
#if defined(CERNLIB_CSPACK)
         ENDIF
#endif
      ELSE
#if defined(CERNLIB_IBM)
         IF(CHFILE(1:1).NE.'/') THEN
            CHFNAM = '/' // CHFILE(1:LFILE)
         ELSE
            CHFNAM = CHFILE(1:LFILE)
         ENDIF
#endif
#if !defined(CERNLIB_IBM)
         CHFNAM = CHFILE(1:LFILE)
#endif
#if defined(CERNLIB_UNIX)
         IF(IOPTC.EQ.0) CALL CUTOL(CHFNAM(1:LFILE))
#endif
 
      ENDIF
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.EQ.' '.OR.JFILE.NE.0) THEN
#endif
*
*     Does file exist?
*
         INQUIRE(FILE=CHFNAM(1:LFILE),EXIST=IEXIST)
 
         IF(.NOT.IEXIST) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMNICK. names file ', 'does not '
     +      //'exist (',CHFNAM(1:LFILE),')'
            IRC = 28
            RETURN
         ENDIF
 
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
 
      LNICK  = LENOCC(CHNICK)
      LSLASH = INDEX(CHNICK(1:LNICK),'/')
      IF(LSLASH.NE.0) THEN
         IF(CHNICK(LSLASH+1:LSLASH+1).EQ.'(') THEN
            CHRNGE = CHNICK(LSLASH+1:LNICK)
            LRNGE  = LNICK - LSLASH
         ELSE
            CHRNGE = '(' // CHNICK(LSLASH+1:LNICK) // ')'
            LRNGE  = LNICK - LSLASH + 2
            LMINUS = INDEX(CHRNGE(1:LRNGE),'-')
            IF(LMINUS.NE.0) THEN
               CHRNGE(LMINUS:LMINUS) = ':'
            ELSE
               CHRNGE = '*'
               LRNGE  = 1
            ENDIF
         ENDIF
         IF(CHRNGE(LRNGE:LRNGE).NE.'*') THEN
            LRNGE = LRNGE + 1
            CHRNGE(LRNGE:LRNGE) = '*'
         ENDIF
         LNICK  = LSLASH - 1
      ELSE
         LRNGE = 0
      ENDIF
 
      CHIN(1,1)  = ':NICK'
      CHIN(2,1)  = CHNICK(1:LNICK)
      NIN        = 1
 
      CHOUT(1,1) = ':GNAME'
      CHOUT(2,1) = ' '
 
      CHOUT(1,2) = ':DESC'
      CHOUT(2,2) = ' '
      NOUT       = 2
 
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
         CALL FMNAMF(LUN,CHFNAM,FATNOD,CHIN,NIN,CHOUT,NOUT,IRC)
      ELSE
#endif
 
#if defined(CERNLIB_IBM)
         IF(CHFNAM(1:1).EQ.'/') THEN
            CALL NAMEFD(LUN,CHFNAM(2:),CHIN,NIN,CHOUT,NOUT,IRC)
         ELSE
            CALL NAMEFD(LUN,CHFNAM,CHIN,NIN,CHOUT,NOUT,IRC)
         ENDIF
#endif
         CALL NAMEFD(LUN,CHFNAM,CHIN,NIN,CHOUT,NOUT,IRC)
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
      IF(IRC.EQ.32) GOTO 99
 
      IRC        = 0
      LDESC      = LENOCC(CHOUT(2,2))
      LNAME      = LENOCC(CHOUT(2,1))
 
      IF(LRNGE.EQ.0) THEN
         CHNAME     = CHOUT(2,1)(1:LNAME)
      ELSE
         CHNAME     = CHOUT(2,1)(1:LNAME)//CHRNGE(1:LRNGE)
      ENDIF
 
      CHDESC     = CHOUT(2,2)(1:LDESC)
 
   99 END
 
