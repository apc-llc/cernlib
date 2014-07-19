*
* $Id: fmtagc.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmtagc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMTAGC
*
*     Interface to FMTAGS
*
#include "fatmen/fmpath.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
      CHARACTER*8   CHOPT
      CHARACTER*255 GENAM,GENAM1,CHTAG
#include "fatmen/fatsys.inc"
#include "fatmen/fatinit.inc"
      CHOPT = ' '
      CHTAG = ' '
      CALL KUGETC(GENAM,LGN)
      CALL FMFIXF(GENAM,GENAM1)
      LGN = LENOCC(GENAM1)
      GENAM = GENAM1
      CALL KUGETI(IKEY)
      CALL KUGETS(CHTAG,NTG)
      CALL KUGETC(CHOPT,NCH)
 
      IF(NCH.EQ.0) THEN
         IF((NTG.NE.0).AND.(CHTAG(1:1).EQ.'-')) THEN
            CHOPT = CHTAG
            NCH   = NTG
            NTG   = 1
            CHTAG = ' '
         ELSE
            NCH    = 1
            CHOPT  = ' '
         ENDIF
      ENDIF
 
      CALL VZERO(KEYS,10)
      KEYS(1) = IKEY
      LBANK   = 0
*
*     Get key vector if KEYS(1) specified
*
      IF(IKEY.NE.0) THEN
         CALL FMGBYK(GENAM(1:LGN),LBANK,KEYS,IRC)
         IF(IRC.NE.0) THEN
             IF(IDEBFA.GE.-3) PRINT *,'FMTAGC. return code ',IRC,
     +          ' from FMGBYK'
             RETURN
         ENDIF
      ENDIF
*
*     For option S with NTG = 0, use current generic name
*
      IF((INDEX(CHOPT(1:NCH),'S').NE.0).AND.
     +   (NTG.EQ.0)) THEN
         CHTAG = GENAM
         NTG   = LGN
      ENDIF
 
      CALL FMTAGS(GENAM(1:LGN),LBANK,KEYS,CHTAG,CHOPT,IRC)
      IF(IRC.NE.0) THEN
         PRINT *,'FMTAGC. return code ',IRC,' from FMTAGS'
      ELSE
         IF(INDEX(CHOPT(1:NCH),'G').NE.0) THEN
            LTG = LENOCC(CHTAG)
            IF(LTG.EQ.0) THEN
               PRINT *,'FMTAGC. no tag set'
            ELSE
               PRINT *,'FMTAGC. tag = ',CHTAG(1:LTG)
             ENDIF
         ENDIF
      ENDIF
 
      END
 
