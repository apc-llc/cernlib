*
* $Id: kilogn.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kilogn.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.70/00 03/07/92  09.41.34  by  Alfred Nathaniel
*-- Author :    Jamie Shiers
      SUBROUTINE KILOGN(CHPATH,CHLOGN,CHOPT)
*
*     Called by KULOGN ==> execute logon macro
*

      CHARACTER*(*) CHPATH,CHLOGN,CHOPT
      COMMON/SLATE/ IS(40)
      CHARACTER*255 CHNAME
      CHARACTER*255 CHFILE
      LOGICAL       IEXIST
#if defined(CERNLIB_VMS)
      INCLUDE '($LNMDEF)'
      INCLUDE '($RMSDEF)'
      STRUCTURE /ITMLST/
         UNION
            MAP
            INTEGER*2 BUFFER_LENGTH
            INTEGER*2 ITEM_CODE
            INTEGER*4 BUFFER_ADDRESS
            INTEGER*4 RETURN_LENGTH_ADDRESS
            ENDMAP
            MAP
            INTEGER*4 END_LIST /0/
            ENDMAP
         END UNION
      END STRUCTURE

      RECORD /ITMLST/ LNM_LIST(3)

      INTEGER SYS$CRELNM
#endif

      LPATH = LENOCC(CHPATH)
      LLOGN = LENOCC(CHLOGN)
      IOPTV = INDEX(CHOPT,'V')
*
*     Search for logon file and execute if found
*
#if defined(CERNLIB_VMS)
*
*     Is CHPATH a logical name?
*
      ISTAT  = LIB$SYS_TRNLOG(CHPATH(1:LPATH),LNAME,CHNAME,,,)
      IF(ISTAT.NE.1) THEN
*
*     define logical name in job table
*
         LNM_LIST(1).BUFFER_LENGTH           = 11
         LNM_LIST(1).ITEM_CODE               = LNM$_STRING
         LNM_LIST(1).BUFFER_ADDRESS          = %LOC('SYS$DISK:[]')
         LNM_LIST(1).RETURN_LENGTH_ADDRESS   = %LOC(LENRET)

         LNM_LIST(2).BUFFER_LENGTH           = 9
         LNM_LIST(2).ITEM_CODE               = LNM$_STRING
         LNM_LIST(2).BUFFER_ADDRESS          = %LOC('SYS$LOGIN')
         LNM_LIST(2).RETURN_LENGTH_ADDRESS   = %LOC(LENRET)

         LNM_LIST(3).END_LIST                = 0

         ISTAT=SYS$CRELNM(,'LNM$JOB',CHPATH(1:LPATH),,LNM_LIST)
      ENDIF

      CHFILE = CHPATH(1:LPATH) // ':' // CHLOGN(1:LLOGN) // '.KUMAC'
#endif
#if defined(CERNLIB_IBMMVS)
      CHFILE = '/' // CHPATH(1:LPATH) // '.'
     +             // CHLOGN(1:LLOGN) // '.KUMAC'
#endif
#if defined(CERNLIB_IBMVM)
      CHFILE = '/' // CHLOGN(1:LLOGN) // ' KUMAC *'
#endif
      LFILE  = LENOCC(CHFILE)
#if defined(CERNLIB_IBMVM)
      INQUIRE(FILE=CHFILE(1:LFILE),NAME=CHNAME,EXIST=IEXIST)
*
*     Check disk mode
*
      IF(IEXIST) THEN
         LNAME = LENOCC(CHNAME)
         IF(INDEX(CHPATH(1:LPATH),CHNAME(LNAME-1:LNAME-1)).NE.0) THEN
            IF(IOPTV.NE.0) PRINT *,'>>> executing ',CHNAME(2:LNAME)
               CALL KUEXEC('EXEC '//CHNAME(2:LNAME))
         ELSE
            IF(IOPTV.NE.0) PRINT *,'>>> macro ',CHLOGN(1:LLOGN),
     +         ' not found on any of following disks: ',CHPATH(1:LPATH)
         ENDIF
      ELSE
         IF(IOPTV.NE.0) PRINT *,'>>> macro ',CHLOGN(1:LLOGN),
     +      ' not found'
      ENDIF
#endif
#if defined(CERNLIB_VMS)
      ISTAT = LIB$FIND_FILE(CHFILE(1:LFILE),CHNAME,ICONT,,,,)
      CALL LIB$FIND_FILE_END(ICONT)
      IF(ISTAT.EQ.RMS$_SUC) THEN
         LNAME = LENOCC(CHNAME)
         IF(IOPTV.NE.0) PRINT *,'>>> executing ',CHNAME(1:LNAME)
         CALL KUEXEC('EXEC '//CHNAME(1:LNAME))
#endif
#if defined(CERNLIB_IBMMVS)
      INQUIRE(FILE=CHFILE(1:LFILE),EXIST=IEXIST)
      IF(IEXIST) THEN
         IF(IOPTV.NE.0) PRINT *,'>>> executing ',CHFILE(2:LFILE)
         CALL KUEXEC('EXEC '//CHFILE(2:LFILE))
#endif
#if defined(CERNLIB_VMS)||defined(CERNLIB_IBMMVS)
      ELSE
         IF(IOPTV.NE.0) PRINT *,'>>> macro ',CHLOGN(1:LLOGN),
     +      ' not found'
      ENDIF
#endif
#if defined(CERNLIB_UNIX)
*
*     Loop over all elements in path name
*
      ISTART = 1
10    CONTINUE
      ICOLON = INDEX(CHPATH(ISTART:LPATH),':')
      IF(ICOLON.NE.0) THEN
         IEND = ISTART + ICOLON - 2
      ELSE
         IEND = LPATH
      ENDIF
*
*     In case of a dot, get CWD
*
      IF(CHPATH(ISTART:IEND).EQ.'.') THEN
         CALL GETWDF(CHNAME)
         CHFILE = CHNAME(1:IS(1))    // '/'
     +            // CHLOGN(1:LLOGN) // '.KUMAC'
      ELSE
         CHFILE = CHPATH(ISTART:IEND) // '/'
     +            // CHLOGN(1:LLOGN)  // '.KUMAC'
      ENDIF
      LFILE = LENOCC(CHFILE)
      CALL CUTOL(CHFILE(1:LFILE))
*
*     Does this file exist?
*
      INQUIRE(FILE=CHFILE(1:LFILE),EXIST=IEXIST)
      IF(IEXIST) THEN
         IF(IOPTV.NE.0) PRINT *,'>>> executing ',CHFILE(1:LFILE)
         CALL KUEXEC('EXEC '//CHFILE(1:LFILE))
         RETURN
      ENDIF

      IF(ICOLON.NE.0) THEN
         ISTART  = ISTART + ICOLON
         GOTO 10
      ENDIF

      IF(IOPTV.NE.0) PRINT *,'>>> macro ',CHLOGN(1:LLOGN),
     +   ' not found'

#endif

      END
