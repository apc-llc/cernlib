*
* $Id: fmuniq.F,v 1.1.1.1 1996/03/07 15:18:11 mclareni Exp $
*
* $Log: fmuniq.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMUNIQ(PATH,KEYSIN,FILES,MAXFIL,NMATCH,CHOPT,IRC)
*CMZ :          23/01/91  10.44.19  by  Jamie Shiers
*-- Author :    Jamie Shiers   23/01/91
*     Routine to return a list of unique file names in the
*     specified directory.
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      CHARACTER*(*) PATH,FILES(MAXFIL)
      CHARACTER*20  FNAME
      CHARACTER*255 CWD,PATHN
#include "fatmen/fmnkeys.inc"
      PARAMETER     (NMAX=100)
      DIMENSION     MYVECT(LKEYFA)
      DIMENSION     KEYSIN(LKEYFA)
      DIMENSION     MYKEYS(LKEYFA,NMAX)
#include "fatmen/fatopts.inc"
*
*     NMAX limits the maximum number of keys that can be processed
*     in a single call to FMKEYS
*
      IRC  = 0
*
*     Save current directory
*
      CALL FACDIR(CWD,'R')
*
*     Reset current directory
*
      LPATH = LENOCC(PATH)
      PATHN = PATH
      CALL FACDIR(PATH(1:LPATH),' ')
 
      IF(IQUEST(1) .NE. 0) THEN
        IRC = -1
        GOTO 999
        ENDIF
*
*     Build compare vector
*
      CALL VZERO(MYVECT,LKEYFA)
      CALL UCOPY(KEYSIN(MKCLFA),MYVECT(MKCLFA),LKEYFA-MKCLFA+1)
      IFIRST = 1
      ILAST  = NMAX
      NDONE  = 0
      NMATCH = 0
1     CONTINUE
      CALL FMKEYS(LKEYFA,NMAX,IFIRST,ILAST,MYKEYS,NFILES,IRET)
      NKEYS  = IQUEST(11)
      IF(IQUEST(1) .NE. 0) THEN
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMUNIQ. More than ',NMAX,' files in ',PATH(1:LPATH)
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMUNIQ. IQUEST(11-12) = ',IQUEST(11),IQUEST(12)
         ENDIF
*
*     Process all keys returned and move those that match to KEYSOU
*
      NRET = IQUEST(13)
      DO 10 I=1,NRET
*
*     Check media type, copy level and location code
*     (essentially just keys(7-9)
*
      DO 20 J=MKMTFA,MKLCFA
*
*     Don't compare negative fields
*
      IF((MYVECT(J) .LT. 0) .AND. (J.GE.MKCLFA)) GOTO 20
      IF(MYVECT(J) .NE. MYKEYS(J,I))             GOTO 10
20    CONTINUE
      FNAME = ' '
      CALL UHTOC(MYKEYS(MKFNFA,I),4,FNAME,(MKCLFA-MKFNFA)*4)
*
*     Have we already got this file name?
*
      JX = ICNTH(PATHN(1:LPATH)//'/'//FNAME,FILES,NDONE)
      IF(JX.NE.0) GOTO 10
*
*     Can we accept any more keys?
*
      NMATCH = NMATCH + 1
      IF(NDONE .LT. MAXFIL) THEN
         NDONE  = NDONE + 1
         FILES(NDONE) = PATHN(1:LPATH)//'/'//FNAME
         ELSE
         IRC    = 1
         ENDIF
 
10    CONTINUE
      IF(ILAST.LT.NKEYS) THEN
         IFIRST = IFIRST + NMAX
         ILAST  = MIN(NKEYS,ILAST+NMAX)
         GOTO 1
         ENDIF
 
999   CALL FACDIR(CWD(1:LENOCC(CWD)),' ')
      IQUEST(11) = NMATCH
      IQUEST(12) = NKEYS
      END
