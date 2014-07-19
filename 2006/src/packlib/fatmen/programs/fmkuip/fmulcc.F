*
* $Id: fmulcc.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmulcc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMULCC
*
*     Lock VID corresponding to specified generic name
*
#include "fatmen/fmpath.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
      PARAMETER   (LKEYFA=10)
#include "fatmen/fmaxcop.inc"
#include "fatmen/fatsys.inc"
      DIMENSION   KEYS(LKEYFA)
      DIMENSION   KEYSIN(LKEYFA)
      DIMENSION   KEYSOU(LKEYFA,MAXCOP)
      CHARACTER*255 GENAM
      CHARACTER*8   CHOPT
#include "fatmen/fatinit.inc"
      CALL KUGETC(GENAM,LGN)
      CALL FMFIXF(GENAM,GENAM)
      LGN = LENOCC(GENAM)
      CALL KUGETI(KSN)
      KEYS(1) = KSN
      CALL KUGETC(CHOPT,NCH)
      LBANK   = 0
      CALL RZCDIR(CDIR(1:LCDIR),'R')
 
      LFILE = INDEXB(GENAM(1:LGN),'/') -1
      FILE1 = GENAM(LFILE+2:LGN)
      CALL RZCDIR(GENAM(1:LFILE),'U')
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
      CALL FMSELK(GENAM(1:LGN),KEYSIN,KEYSOU,NMATCH,MAXCOP,IRC)
      IF(NMATCH.EQ.0) THEN
         IF(IDEBFA.GE.0)
     +   PRINT *,'FMULCC. found 0 matches for ',GENAM(1:LGN)
         IRC = 1
         GOTO 99
      ELSEIF((NMATCH.GT.1).AND.(KEYS(1).EQ.0)) THEN
         IF(IDEBFA.GE.0) THEN
            PRINT *,'FMULCC. found ',NMATCH,' matches for ',
     +      GENAM(1:LGN)
            PRINT *,'FMULCC. Please specify which copy is to be locked'
         ENDIF
         IRC = 1
         GOTO 99
      ELSE
         DO 10 I=1,NMATCH
   10    IF(KEYS(1).EQ.KEYSOU(1,I)) GOTO 20
   20    CONTINUE
         CALL FMGETK(GENAM(1:LGN),LTDSFA,KEYSOU(1,I),IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMULCC. Return code ',IRC,' from '
     +      //'FMGETK'
            GOTO 99
         ENDIF
      ENDIF
 
      CALL FMULOK(GENAM(1:LGN),LTDSFA,KEYS,CHOPT,IRC)
      IF(IRC.NE.0) PRINT *,'Return code ',IRC,' from FMULOK'
   99 CONTINUE
*
*     Reset current directory
*
      CALL RZCDIR(CDIR(1:LCDIR),' ')
 
      END
 
