*
* $Id: fmfils.F,v 1.2 1997/07/04 13:36:31 jamie Exp $
*
* $Log: fmfils.F,v $
* Revision 1.2  1997/07/04 13:36:31  jamie
* save patho
*
* Revision 1.1.1.1  1996/03/07 15:18:12  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFILS(PATH,FILES,KEYS,NDONE,MAXKEY,ICONT,IRC)
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fmnkeys.inc"
      CHARACTER*(*) PATH
      CHARACTER*255 PATHO
      CHARACTER*(*) FILES(MAXKEY)
      CHARACTER*20  FNAME,MATCH
      DIMENSION     KEYS(LKEYFA,MAXKEY)
*
*     NMAX limits the maximum number of keys that can be processed
*     in a single call to FMKEYS
*
      PARAMETER     (NMAX=100)
      DIMENSION     MYKEYS(LKEYFA,NMAX)
      SAVE          ISTART,NKEYS,NRET,IFIRST,ILAST,MYKEYS,PATHO
 
      IRC    = 0
      LPATH  = LENOCC(PATH)
      LP     = INDEXB(PATH(1:LPATH),'/')-1
      MATCH  = PATH(LP+2:LPATH)
      LM     = LENOCC(MATCH)
      IF(IDEBFA.GE.2) THEN
         PRINT *,'FMFILS. enter for path = ',PATH(1:LP),' match = ',
     +           MATCH(1:LM)
         PRINT *,'FMFILS. icont = ',ICONT,' istart = ',ISTART
      ENDIF
 
      NDONE  = 0
      IF(ICONT.NE.0) THEN
         IF(IDEBFA.GE.2) PRINT *,'FMFILS. continue at offset ',
     +      ISTART,' in range ',IFIRST,ILAST
         GOTO 20
      ENDIF
 
      ISTART = 1
*
*     Save current directory
*
      CALL FACDIR(PATHO,'R')
*
*     Reset current directory
*
      CALL FACDIR(PATH(1:LP),' ')
 
      IF(IQUEST(1) .NE. 0) THEN
         IRC = -1
         GOTO 40
      ENDIF
 
      IFIRST = 1
      ILAST  = NMAX
 
   10 CONTINUE
      CALL FMKEYS(LKEYFA,NMAX,IFIRST,ILAST,MYKEYS,NFILES,IRET)
      NKEYS  = IQUEST(11)
      IF(IQUEST(1) .NE. 0) THEN
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMFILS. More than ',NMAX,' files in ',PATH(1:LP)
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMFILS. IQUEST(11-12) = ',IQUEST(11),IQUEST(12)
      ENDIF
*
*     Process all keys returned and move those that match to KEYS
*
      NRET = IQUEST(13)
      IF(NRET.EQ.0) GOTO 40
   20 CONTINUE
      DO 30 I=ISTART,NRET
*
*     Can we accept any more keys?
*
         IF(NDONE .LT. MAXKEY) THEN
            CALL UHTOC(MYKEYS(2,I),4,FNAME,(MKCLFA-MKFNFA)*4)
            CALL FMATCH(FNAME,MATCH(1:LM),IRET)
            IF(IRET.EQ.0) THEN
               NDONE = NDONE + 1
               FILES(NDONE) = PATH(1:LP)//'/'//FNAME
               CALL UCOPY(MYKEYS(1,I),KEYS(1,NDONE),LKEYFA)
            ENDIF
         ELSE
            IRC    = -1
            ICONT  = 1
            ISTART = I
            RETURN
         ENDIF
 
   30 CONTINUE
      IF(ILAST.LT.NKEYS) THEN
         IFIRST = IFIRST + NMAX
         ILAST  = MIN(NKEYS,ILAST+NMAX)
         ISTART = 1
         GOTO 10
      ENDIF
 
   40 CALL FACDIR(PATHO(1:LENOCC(PATHO)),' ')
      END
