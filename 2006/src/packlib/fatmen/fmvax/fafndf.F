*
* $Id: fafndf.F,v 1.1.1.1 1996/03/07 15:18:23 mclareni Exp $
*
* $Log: fafndf.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:23  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      INTEGER FUNCTION FAFNDF(CHPATH,CHFILE,ICONT)
*
*     FORTRAN interface to LIB$FIND_FILE
*
      CHARACTER*(*) CHPATH,CHFILE
      CHARACTER*255 CHMESS,CHWTOM,EQUNAM
      INTEGER       STATUS
      INTEGER       SYS$GET_MSG
      INCLUDE       '($SSDEF)'
      PARAMETER     (DEFSNO=60.)
      PARAMETER     (DEFSLP=1800.)
      PARAMETER     (DEFTRY=10)
#include "fatmen/slate.inc"
#include "fatmen/fatbug.inc"
*
*     Get short and long wait times and retry count
*
      CALL FMGTLG('FATMEN_SNETWT',EQUNAM,'LNM$_SYSTEM',IRC)
      IF(IRC.EQ.0) THEN
         IVAL   = ICDECI(EQUNAM,1,IS(1))
         SNOOZE = IVAL
      ELSE
         SNOOZE = DEFSNO
      ENDIF
 
      CALL FMGTLG('FATMEN_LNETWT',EQUNAM,'LNM$_SYSTEM',IRC)
      IF(IRC.EQ.0) THEN
         IVAL   = ICDECI(EQUNAM,1,IS(1))
         SLEEP  = IVAL
      ELSE
         SLEEP  = DEFSLP
      ENDIF
 
      CALL FMGTLG('FATMEN_RETRY' ,EQUNAM,'LNM$_SYSTEM',IRC)
      IF(IRC.EQ.0) THEN
         MAXTRY = ICDECI(EQUNAM,1,IS(1))
      ELSE
         MAXTRY = DEFTRY
      ENDIF
 
      NTRY   = 0
      WTTIME = SNOOZE
      LPATH  = LENOCC(PATH)
   10 CONTINUE
      NTRY   = NTRY + 1
      FAFNDF = LIB$FIND_FILE(CHPATH,CHFILE,ICONT,,,ISTAT,)
      ICODE  = LIB$FIND_FILE_END(ICONT)
      IF(ISTAT.NE.SS$_UNREACHABLE) RETURN
 
      IF(IDEBFA.GE.0) THEN
         PRINT 9001,ISTAT
 9001 FORMAT(' FAFNDF. return code ',I10,' from LIB$FIND_FILE')
         ICODE = SYS$GETMSG(%VAL(ISTAT),LMESS,CHMESS,,)
         PRINT 9002,CHMESS(1:LMESS)
 9002 FORMAT(' FAFNDF. message text = ',A)
      ENDIF
 
      IF(NTRY.LE.MAXTRY) THEN
         IF(IDEBFA.GE.0) PRINT 9003,WTTIME,NTRY
 9003 FORMAT(' FAFNDF. sleeping for ',F5.0,
     +       ' seconds before retry number ',I3)
         IF(WTTIME.EQ.SLEEP) THEN
            CHWTOM = ' '
            WRITE(CHWTOM,9004) CHMESS(1:LMESS),CHPATH(1:LPATH)
 9004 FORMAT('FATMEN-W-FAFNDF, error ',A,' accessing ',A)
            LWTOM = LENOCC(CHWTOM)
            CALL FMWTO(CHWTOM,LWTOM,IRC)
         ENDIF
      ELSE
         IF(WTTIME.EQ.SNOOZE) THEN
            NTRY = 1
            WTTIME = SLEEP
            IF(IDEBFA.GE.0) PRINT 9005,WTTIME,MAXTRY
 9005 FORMAT(' FAFNDF. switching to long sleep ',F5.0,
     +       ' after ',I3,' retries at ',F5.0,' seconds')
            CHWTOM = ' '
            WRITE(CHWTOM,9004) CHMESS(1:LMESS),CHPATH(1:LPATH)
            LWTOM = LENOCC(CHWTOM)
            CALL FMWTO(CHWTOM,IWTOM,IRC)
            ICODE = LIB$WAIT(WTTIME)
            GOTO 10
         ELSE
            IF(IDEBFA.GE.0) PRINT 9006,MAXTRY
 9006 FORMAT(' FAFNDF. giving up after ',I3,' retries')
         ENDIF
      ENDIF
 
      IF(NTRY.GT.MAXTRY) THEN
         FAFNDF = ISTAT
         RETURN
      ENDIF
 
      ICODE = LIB$WAIT(WTTIME)
      GOTO 10
 
      END
