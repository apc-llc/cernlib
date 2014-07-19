*
* $Id: fmselk.F,v 1.1.1.1 1996/03/07 15:18:09 mclareni Exp $
*
* $Log: fmselk.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:09  mclareni
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
#include "fatmen/faust.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatpara.inc"
#include "zebra/rzcl.inc"
#include "zebra/rzk.inc"
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
      DIMENSION     MYKEYS(LKEYFA,NMAX),KEYTMP(LKEYFA)
 
      IRC = 0
 
      NFSELK = NFSELK + 1
*
*     Check for wild-cards in generic-name
*
      LGN   = LENOCC(GENAM)
      IWILD = ICFMUL('*%(<>[]',GENAM,1,LGN)
      IF(IWILD.LE.LGN) THEN
         PATH = ' '
         PATH(IWILD:IWILD+3) = '^---'
         WRITE(LPRTFA,9001) GENAM(1:LGN),PATH(1:IWILD+3)
9001  FORMAT(' FMSELK. wild-cards not permitted in generic name',/,
     +       1X,A,/,1X,A)
         IRC = 13
         RETURN
      ENDIF
 
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
      CALL FACDIR(GENAM(1:LGEN-1),'U')
      IF(IQUEST(1).NE.0) GOTO 80
 
      IF(LQRS.EQ.0)  GOTO 80
      IF(LCDIR.EQ.0) GOTO 80
      LS    = IQ(KQSP+LCDIR+KLS)
      LK    = IQ(KQSP+LCDIR+KLK)
      NK    = IQ(KQSP+LCDIR+KNKEYS)
      NWK   = IQ(KQSP+LCDIR+KNWKEY)
      NKEYS = NK
      NWKEY = NWK
*
*     Build compare vector
*
      CALL VZERO(MYVECT,LKEYFA)
      CALL UCOPY(KEYSIN(MKCLFA),MYVECT(MKCLFA),LKEYFA-MKCLFA+1)
      IFIRST = 1
      ILAST  = NMAX
      NDONE  = 0
*
* ***  Find file name
*
      NCH    = LENOCC(GENAM)
      FNAME  = GENAM(LGEN+1:NCH)
      CALL VBLANK(MYVECT(2),5)
      CALL UCTOH(FNAME,MYVECT(2),4,MIN(NCH-LGEN,20))
 
      IF(IDEBFA.GE.3) PRINT *,'FMSELK. enter for ',GENAM(1:NCH)
      IF(IDEBFA.GE.3) CALL FMPKEY(MYVECT,LKEYFA)
*
*     Convert file name to Z format
*
      CALL ZHTOI(MYVECT(MKFNFA),MYVECT(MKFNFA),MKCLFA-MKFNFA)
   10 CONTINUE
*
*     Process all keys returned and move those that match to KEYSOU
*
      NDONE = 0
      DO 60 I=1,NKEYS
 
         IF(NDONE.GE.MAXKEY) THEN
            IRC = 1
            GOTO 80
         ENDIF
 
         NDONE = NDONE + 1
         K = LK+(NWK+1)*(I-1)
*
*     Find end of range
*
         DO 20 J=NWK-1,MKCLFA,-1
            IF(MYVECT(J).NE.-1) GOTO 30
   20    CONTINUE
   30    JEND = MIN(MKLCFA,J)
*
*     Check file name, media type, copy level and location code
*     (essentially just keys(2-9)
*
         DO 40 J=MKFNFA,JEND
*
*     Don't compare negative fields
*
            IF(MYVECT(J).LT.0.AND.J.GE.MKCLFA) GOTO 40
            IF(MYVECT(J).NE.IQ(KQSP+LCDIR+K+J)) THEN
               NDONE = NDONE - 1
               GOTO 60
            ENDIF
   40    CONTINUE
 
         DO 50 J=1,NWK
            IKDES=(J-1)/10
            IKBIT1=3*J-30*IKDES-2
            IF(JBYT(IQ(KQSP+LCDIR+KKDES+IKDES),IKBIT1,3).LT.3)THEN
               KEYSOU(J,NDONE)=IQ(KQSP+LCDIR+K+J)
            ELSE
               CALL ZITOH(IQ(KQSP+LCDIR+K+J),KEYSOU(J,NDONE),1)
            ENDIF
   50    CONTINUE
 
         IF(IDEBFA.GE.3) THEN
            PRINT *,'FMSELK. match # ',NDONE
            CALL FMPKEY(KEYSOU(1,NDONE),LKEYFA)
         ENDIF
   60 CONTINUE
 
   70 CALL FACDIR(PATH(1:LENOCC(PATH)),' ')
      IQUEST(11) = NDONE
      IQUEST(12) = NKEYS
      RETURN
   80 IRC = -1
      END
