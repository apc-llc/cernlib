*
* $Id: fmedia.F,v 1.1.1.1 1996/03/07 15:18:15 mclareni Exp $
*
* $Log: fmedia.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:15  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMEDIA(MFMMED,MFMTYP,MFMGEN,MFMSIZ,MFMDEN,
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
      NFMTYP = NMEDIA
      DO 10 I=1,NMEDIA
         J = MFMMED(I)
         IF(J.GT.NMTYP) THEN
            IF(IDEBFA.GE.-3) PRINT 9001,J,NMTYP
 9001 FORMAT(' FMEDIA. media type ',I10,
     +      ' outside range. Maximum allowed value = ',I10)
            GOTO 10
         ENDIF
*
*     Physical device type
*
         CHMTYP(J) = MFMTYP(I)
         IF(IDEBFA.GE.3) WRITE(LPRTFA,9002) J,MFMTYP(I)
 9002 FORMAT(' FMEDIA. setting physical device type ',I6,' to ',A)
*
*     Generic device type
*
         CHMGEN(J) = MFMGEN(I)
         IF(IDEBFA.GE.3) WRITE(LPRTFA,9003) J,MFMGEN(I)
 9003 FORMAT(' FMEDIA. setting generic device type ',I6,' to ',A)
*
*     Capacity
*
         CHMSIZ(J) = MFMSIZ(I)
         MEDSIZ(J) = ICDECI(MFMSIZ(I),1,LENOCC(MFMSIZ(I)))
         IF(IDEBFA.GE.3) WRITE(LPRTFA,9004) J,MFMSIZ(I)
 9004 FORMAT(' FMEDIA. setting capacity of type ',I6,' to ',A)
*
*     Density
*
         CHMDEN(J) = MFMDEN(I)
         MEDDEN(J) = ICDECI(MFMDEN(I),1,LENOCC(MFMDEN(I)))
         IF(INDEX(MFMDEN(I),'K').NE.0) MEDDEN(J) = MEDDEN(J) * 1000
         IF(IDEBFA.GE.3) WRITE(LPRTFA,9005) J,MFMDEN(I)
 9005 FORMAT(' FMEDIA. setting density of type ',I6,' to ',A)
*
*     Label type
*
         CHMLAB(J) = MFMLAB(I)
         IF(IDEBFA.GE.3) WRITE(LPRTFA,9006) J,MFMLAB(I)
 9006 FORMAT(' FMEDIA. setting label type of type ',I6,' to ',A)
*
*     Mount type
*
         CHMMNT(J) = MFMMNT(I)
         IF(IDEBFA.GE.3) WRITE(LPRTFA,9007) J,MFMMNT(I)
 9007 FORMAT(' FMEDIA. setting mount type of type ',I6,' to ',A)
*
   10 CONTINUE
      END
