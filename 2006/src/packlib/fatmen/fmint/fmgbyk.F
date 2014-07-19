*
* $Id: fmgbyk.F,v 1.1.1.1 1996/03/07 15:18:11 mclareni Exp $
*
* $Log: fmgbyk.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMGBYK(GENAM,LBANK,KEYS,IC)
*CMZ :          25/01/91  14.36.59  by  Jamie Shiers
*-- Author :    Jamie Shiers   25/01/91
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fmnkeys.inc"
      PARAMETER     (NMAX=100)
      DIMENSION     KEYS(LKEYFA)
      DIMENSION     MYKEYS(LKEYFA,NMAX)
      CHARACTER*(*) GENAM
      CHARACTER*255 PATH
      CHARACTER*20  FNAME
 
      IRC  = 0
*
*     Save current directory
*
      CALL FACDIR(PATH,'R')
*
*     Reset current directory
*
      LGEN = INDEXB(GENAM,'/')
      CALL FACDIR(GENAM(1:LGEN-1),' ')
 
      IF(IQUEST(1) .NE. 0) THEN
         IRC = -1
         GOTO 999
      ENDIF
*
*     Build compare vector
*
      IFIRST = 1
      ILAST  = NMAX
      NDONE  = 0
      NMATCH = 0
      IMATCH = 0
*
* ***  Find file name
*
      NCH    = LENOCC(GENAM)
      FNAME  = GENAM(LGEN+1:NCH)
      IF(IDEBFA.GE.3) PRINT *,'FMGBYK. Enter for ',GENAM(1:NCH),
     +   ' key serial number = ',KEYS(1)
      CALL VBLANK(KEYS(2),5)
      CALL UCTOH(FNAME,KEYS(2),4,MIN(NCH-LGEN,20))
 
      IF(KEYS(1).EQ.0) THEN
         IMATCH = 1
         GOTO 100
      ENDIF
 
      IF(IDEBFA.GE.4) THEN
         PRINT *,'FMGBYK. compare vector ...'
         CALL FMPKEY(KEYS,LKEYFA)
      ENDIF
 
    1 CONTINUE
      CALL FMKEYS(LKEYFA,NMAX,IFIRST,ILAST,MYKEYS,NFILES,IRET)
      NKEYS  = IQUEST(11)
      IF(IQUEST(1) .NE. 0) THEN
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMGBYK. More than ',NMAX,' files in ',GENAM(1:LGEN-1)
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMGBYK. IQUEST(11-12) = ',IQUEST(11),IQUEST(12)
      ENDIF
*
*     Process all keys returned and move the match to KEYS
*
      NRET = IQUEST(13)
      DO 10 I=1,NRET
*
*     Check Key serial number and file name
*
         IF(IDEBFA.GE.4) THEN
            PRINT *,'FMGBYK. candidate # ',I,'...'
            CALL FMPKEY(MYKEYS(1,I),LKEYFA)
         ENDIF
 
         DO 20 J=MKSRFA,MKCLFA-1
            IF(KEYS(J) .NE. MYKEYS(J,I)) GOTO 10
   20    CONTINUE
         CALL UCOPY(MYKEYS(1,I),KEYS(1),LKEYFA)
         IF(IDEBFA.GE.3) THEN
            PRINT *,'FMGBYK. found match...'
            CALL FMPKEY(KEYS,LKEYFA)
         ENDIF
         IMATCH = 1
         GOTO 100
 
   10 CONTINUE
      IF(ILAST.LT.NKEYS) THEN
         IFIRST = IFIRST + NMAX
         ILAST  = MIN(NKEYS,ILAST+NMAX)
         GOTO 1
      ENDIF
  100 CONTINUE
      IF(IMATCH.EQ.0) THEN
         IC = 13
         IF(IDEBFA.GE.0) PRINT *,'FMGBYK. no match for ',GENAM(1:NCH),
     +   ' key serial number = ',KEYS(1),' found'
      ELSE
         CALL FMGETK(GENAM(1:NCH),LBANK,KEYS,IC)
      ENDIF
 
  999 CALL FACDIR(PATH(1:LENOCC(PATH)),' ')
      END
