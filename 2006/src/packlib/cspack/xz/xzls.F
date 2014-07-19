*
* $Id: xzls.F,v 1.1.1.1 1996/03/08 15:44:31 mclareni Exp $
*
* $Log: xzls.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:31  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZLS(PATH,FILES,NMAX,NFILES,ICONT,CHOPT,IRC)
#include "cspack/czsock.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
      CHARACTER*(*) PATH,FILES(NMAX)
      CHARACTER*255 CHPATH
      CHARACTER*8   CHOPTT
*
#include "cspack/czopts.inc"
#include "cspack/czopen.inc"
      IRC    = 0
      NFILES = 0
      IF(ICONT.NE.0) GOTO 10
      LPATH  = LENOCC(PATH)
      NCH    = LENOCC(CHOPT)
 
      IF(LPATH.EQ.0) THEN
         CHPATH = ' '
         LPATH  = 1
      ELSE
         CHPATH = PATH(1:LPATH)
      ENDIF
 
      CHOPTT   = CHOPT
      IF(NCH.EQ.0) THEN
         CHOPTT = ' '
         NCH    = 1
      ENDIF
*
      CHMAIL = 'XZIO :LS  '//CHPATH(1:LPATH) // '('
     +         // CHOPTT(1:NCH) // ')'
 
      CALL CZPUTA(CHMAIL,ISTAT)
*
*     Process server messages
*
   10 CONTINUE
      CALL CZGETA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0) THEN
         IRC = 99
         GOTO 99
      ENDIF
 
      LCH = LENOCC(CHMAIL)
      IF(CHMAIL(1:1).EQ.'0') THEN
*
*        Nop
*
      ELSEIF(CHMAIL(1:1).EQ.'1') THEN
         PRINT *,CHMAIL(2:LCH)
      ELSEIF(CHMAIL(1:1).EQ.'2') THEN
         NFILES = NFILES + 1
         IF(NFILES.LE.NMAX) THEN
            FILES(NFILES) = CHMAIL(2:LCH)
         ELSE
            IRC   = -1
            ICONT = 1
            RETURN
         ENDIF
         GOTO 10
      ELSEIF(CHMAIL(1:1).EQ.'3') THEN
         PRINT *,CHMAIL(2:LCH)
         IQUEST(1) = 1
         IRC       = 1
      ELSE
         PRINT *,'Unknown server message ',CHMAIL
         IQUEST(1) = 1
         IRC       = 1
      ENDIF
*
   99 END
