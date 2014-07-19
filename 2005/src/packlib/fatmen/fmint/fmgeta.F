*
* $Id: fmgeta.F,v 1.1.1.1 1996/03/07 15:18:14 mclareni Exp $
*
* $Log: fmgeta.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:14  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMGETA(GENAME,LBANK,KEYS,IRC)
#include "fatmen/faust.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fmpath.inc"
*
*     If key serial number is non-zero, use that
*
      CHARACTER*(*) GENAME
      PARAMETER   (LKEYFA=10)
#include "fatmen/fmaxcop.inc"
      DIMENSION   KEYS(LKEYFA)
      DIMENSION   KEYSIN(LKEYFA)
      DIMENSION   KEYSOU(LKEYFA,MAXCOP)
 
      IRC  = 0
      NCH  = LENOCC(GENAME)
 
      IF(LTDSFA.NE.0) THEN
         CALL MZDROP(IDIVFA,LTDSFA,'L')
         LTDSFA = 0
      ENDIF
 
      LFILE = INDEXB(GENAME(1:NCH),'/') -1
      FILE1 = GENAME(LFILE+2:NCH)
      CALL FACDIR(GENAME(1:LFILE),'U')
      LFILE = LENOCC(FILE1)
*
*     Check how many copies of this dataset exist
*
      CALL UCOPY(KEYS,KEYSIN,10)
*
*     Don't compare media type, copy level or location code
*
      KEYSIN(MKMTFA) = -1
      KEYSIN(MKCLFA) = -1
      KEYSIN(MKLCFA) = -1
      CALL FMSELK(GENAME(1:NCH),KEYSIN,KEYSOU,NMATCH,MAXCOP,IRC)
      IF(NMATCH.EQ.0) THEN
         IF(IDEBFA.GE.0)
     +   PRINT *,'FMGETA. found 0 matches for ',GENAME(1:NCH)
         IRC = 1
         GOTO 99
      ELSE
         LTDSFA = 0
         CALL FMGETK(GENAME(1:NCH),LTDSFA,KEYSOU(1,1),IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMGETA. Return code ',IRC,' from '
     +      //'FMGETK'
            GOTO 99
         ENDIF
         CALL UCOPY(KEYSOU(1,1),KEYS,10)
      ENDIF
 
      LBANK = LTDSFA
      RETURN
 
   99 CONTINUE
      LBANK = 0
      END
