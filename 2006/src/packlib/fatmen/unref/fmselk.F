*
* $Id: fmselk.F,v 1.1.1.1 1996/03/07 15:18:24 mclareni Exp $
*
* $Log: fmselk.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:24  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSELK(GENAM,KEYSIN,KEYSOU,NDONE,MAXKEY,IRC)
*
*     This subroutine matches the input key vector KEYSIN
*     against the keys in the directory specified by the
*     the generic name GENAM. A maximum of MAXKEY key vectors
*     are returned in the arrary KEYSOU. NDONE returns the
*     actual number of vectors returned.
*     IRC =-1 invalid pathname
*         = 0  success
*         = 1  more than NMAX files in CWD
*     IQUEST(11) = number of matching files in CWD
*     IQUEST(12) = total number of files in CWD
*
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
      CHARACTER*(*) GENAM
      CHARACTER*255 PATH
      CHARACTER*20  FNAME
      PARAMETER     (LKEYFA=10)
*
*     NMAX limits the maximum number of keys that can be processed
*     in a single call to FMKEYS
*
      PARAMETER     (NMAX=100)
      DIMENSION     KEYS(LKEYFA),MYVECT(LKEYFA)
      DIMENSION     KEYSIN(LKEYFA),KEYSOU(LKEYFA,MAXKEY)
      DIMENSION     MYKEYS(LKEYFA,NMAX)
 
      IRC  = 0
*
*     Convert generic name to upper case
*
      CALL CLTOU(GENAM)
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
      CALL VZERO(MYVECT,LKEYFA)
      CALL UCOPY(KEYSIN(MKCLFA),MYVECT(MKCLFA),LKEYFA-MKCLFA+1)
      IFIRST = 1
      ILAST  = NMAX
      NDONE  = 0
      NMATCH = 0
*
* ***  Find file name
*
      NCH    = LENOCC(GENAM)
      FNAME  = GENAM(LGEN+1:NCH)
      CALL VBLANK(MYVECT(2),5)
      CALL UCTOH(FNAME,MYVECT(2),4,MIN(NCH-LGEN,20))
      IF(IDEBFA.GE.3) PRINT *,'FMSELK. enter for ',GENAM(1:NCH)
      IF(IDEBFA.GE.3) CALL FMPKEY(MYVECT,LKEYFA)
1     CONTINUE
      CALL FMKEYS(LKEYFA,NMAX,IFIRST,ILAST,MYKEYS,NFILES,IRET)
      NKEYS  = IQUEST(11)
      IF(IQUEST(1) .NE. 0) THEN
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMSELK. More than ',NMAX,' files in ',GENAM(1:LGEN-1)
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMSELK. IQUEST(11-12) = ',IQUEST(11),IQUEST(12)
         ENDIF
*
*     Process all keys returned and move those that match to KEYSOU
*
      NRET = IQUEST(13)
      DO 10 I=1,NRET
*
*     Check file name, media type, copy level and location code
*     (essentially just keys(2-9)
*
      IF(IDEBFA.GE.3) THEN
         PRINT *,'FMSELK. candidate # ',I
         CALL FMPKEY(MYKEYS(1,I),LKEYFA)
      ENDIF
      DO 20 J=MKFNFA,MKLCFA
*
*     Don't compare negative fields
*
      IF((MYVECT(J) .LT. 0) .AND. (J.GE.MKCLFA)) GOTO 20
      IF(MYVECT(J) .NE. MYKEYS(J,I))             GOTO 10
20    CONTINUE
*
*     Can we accept any more keys?
*
      NMATCH = NMATCH + 1
      IF(NDONE .LT. MAXKEY) THEN
         NDONE  = NDONE + 1
         CALL UCOPY(MYKEYS(1,I),KEYSOU(1,NDONE),LKEYFA)
         ELSE
         IRC    = 1
         ENDIF
 
10    CONTINUE
      IF(ILAST.LT.NKEYS) THEN
         IFIRST = IFIRST + NMAX
         ILAST  = MIN(NKEYS,ILAST+NMAX)
         GOTO 1
         ENDIF
 
999   CALL FACDIR(PATH(1:LENOCC(PATH)),' ')
      IQUEST(11) = NMATCH
      IQUEST(12) = NKEYS
      END
 
