*
* $Id: fmamed.F,v 1.1.1.1 1996/03/07 15:18:15 mclareni Exp $
*
* $Log: fmamed.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:15  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMAMED(MFMMED,MFMTYP,MFMGEN,MFMSIZ,MFMDEN,
     +                  MFMMNT,MFMLAB,NMEDIA,IRC)
#include "fatmen/fattyp.inc"
#include "fatmen/fatbug.inc"
*
      DIMENSION   MFMMED(NMEDIA)
      CHARACTER*(*) MFMTYP(NMEDIA),MFMGEN(NMEDIA),MFMSIZ(NMEDIA),
     +              MFMDEN(NMEDIA),MFMMNT(NMEDIA),MFMLAB(NMEDIA)
*CMZ :          01/07/91  12.33.12  by  Jamie Shiers
*-- Author :    Jamie Shiers   01/07/91
      IRC    = 0
      DO 10 I=1,NMEDIA
      J      =  MFMMED(I)
      IF(J.GT.NMTYP) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMEDIA. media type ',J,
     +      ' outside range. Maximum allowed value = ',NMTYP
         GOTO 10
      ENDIF
 
      NFMTYP = NFMTYP + 1
*
*     Physical device type
*
      CHMTYP(NFMTYP) = MFMTYP(J)
*
*     Generic device type
*
      CHMGEN(NFMTYP) = MFMGEN(J)
*
*     Capacity
*
      CHMSIZ(NFMTYP) = MFMSIZ(J)
      MEDSIZ(NFMTYP) = ICDECI(MFMSIZ(J),1,LENOCC(MFMSIZ(J)))
*
*     Density
*
      CHMDEN(NFMTYP) = MFMDEN(J)
      MEDDEN(NFMTYP) = ICDECI(MFMDEN(J),1,LENOCC(MFMDEN(J)))
      IF(INDEX(MFMDEN(NFMTYP),'K').NE.0)
     +   MEDDEN(NFMTYP) = MEDDEN(NFMTYP) * 1000
*
*     Label type
*
      CHMLAB(NFMTYP) = MFMLAB(J)
*
*     Mount type
*
      CHMMNT(NFMTYP) = MFMMNT(J)
*
10    CONTINUE
      END
