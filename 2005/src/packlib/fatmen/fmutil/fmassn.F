*
* $Id: fmassn.F,v 1.1.1.1 1996/03/07 15:18:19 mclareni Exp $
*
* $Log: fmassn.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:19  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_UNIX)
      SUBROUTINE FMASSN(DDNAME,CHFILE,CHOPT,IRC)
 
      CHARACTER*(*) DDNAME,CHFILE
      CHARACTER*255 CHHOME,CHTEMP,CHDDNM,CHNAME,CHLINE
      INTEGER       SYSTEMF
#include "fatmen/slate.inc"
#include "fatmen/fatbug.inc"
*
*
*     Get, Set or Print a file assignment (Cray only?)
#include "fatmen/fatopts.inc"
 
      IRC = 0
 
      LDD = LENOCC(DDNAME)
      LCH = LENOCC(CHFILE)
 
      CHDDNM = DDNAME(1:LDD)
 
      IF(IDEBFA.GE.1) PRINT *,'FMASSN. enter for ',
     +   DDNAME(1:LDD),' ',CHFILE(1:LCH),' ',CHOPT
 
      IF(IOPTP.NE.0) THEN
*
*     Print
*
         IRC = SYSTEMF('assign -V | grep '//CHDDNM(1:LCH))
 
      ELSEIF(IOPTG.NE.0) THEN
*
*     Get (translate)
*
         CHFILE = ' '
*
*     Get a unique filename
*
         CALL FMFNME(CHTEMP)
         LTEMP = LENOCC(CHTEMP)
*
*     Get home directory
*
         CALL GETENVF('HOME',CHHOME)
         LHOME = IS(1)
*
*     Issue assign command
*
         IF(IDEBFA.GE.3) PRINT *,'FMASSN. issuing ',
     +                 'assign -V | grep '//CHDDNM(1:LDD)
     +      //' > '//CHHOME(1:LHOME)//'/'//CHTEMP(1:LTEMP)
 
         IRC = SYSTEMF('assign -V | grep '//CHDDNM(1:LDD)
     +      //' > '//CHHOME(1:LHOME)//'/'//CHTEMP(1:LTEMP))
*
*     Read output file
*
#if defined(CERNLIB_NOCIO)
         CALL FMGLUN(LUNASS,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMASSN. cannot assign ',
     +         'logical unit'
            RETURN
         ENDIF
 
         CALL FAFILE(LUNASS,
     +        CHHOME(1:LHOME)//'/'//CHTEMP(1:LTEMP)
     +        ,ISTAT)
#endif
#if !defined(CERNLIB_NOCIO)
         CALL CIOPEN(LUNASS,'r',
     +        CHHOME(1:LHOME)//'/'//CHTEMP(1:LTEMP)
     +        ,ISTAT)
#endif
   10    CONTINUE
#if defined(CERNLIB_NOCIO)
         READ(LUNASS,'(A)',END=20) CHLINE
         LLINE  = LENOCC(CHLINE)
#endif
#if !defined(CERNLIB_NOCIO)
         CALL FMCFGL(LUNASS,CHLINE,LLINE,' ',IRC)
         IF(IRC.NE.0) GOTO 20
#endif
         IF(IDEBFA.GE.3) PRINT *,'FMASSN. read ',CHLINE(1:LLINE)
*
*     Get actual file name
*
         IFOUND = INDEX(CHLINE(1:LLINE),' -a ')
         IF(IFOUND.EQ.0) GOTO 10
         LEND   = INDEX(CHLINE(IFOUND+4:LLINE),' ')
         IF(LEND.EQ.0) THEN
            LEND = LLINE
         ELSE
            LEND = LEND + IFOUND + 3
         ENDIF
         IF(IDEBFA.GE.3) PRINT *,'FMASSN. actual file is ',
     +      CHLINE(IFOUND+4:LEND)
         CHFILE = CHLINE(IFOUND+4:LEND)
         GOTO 20
   20    CONTINUE
#if defined(CERNLIB_NOCIO)
         CLOSE(LUNASS)
         CALL FMFLUN(LUNASS,IRC)
#endif
#if !defined(CERNLIB_NOCIO)
         CALL FMCFGL(LUNASS,CHLINE,LLINE,'F',IRC)
         CALL CICLOS(LUNASS)
#endif
         IF(IFOUND.EQ.0) IRC = -1
 
         IC = SYSTEMF('rm '//CHHOME(1:LHOME)//'/'//CHTEMP(1:LTEMP))
 
      ELSEIF(IOPTS.NE.0) THEN
*
*     Set
*
         CHNAME = CHFILE(1:LCH)
         IRC = SYSTEMF('assign -a '//CHNAME(1:LCH)//' '
     +      //CHDDNM(1:LCH))
 
      ENDIF
 
      END
#endif
