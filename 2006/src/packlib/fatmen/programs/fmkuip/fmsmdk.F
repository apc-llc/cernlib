*
* $Id: fmsmdk.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmsmdk.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSMDK
*CMZ :          10/07/91  11.45.43  by  Jamie Shiers
*-- Author :    Jamie Shiers   10/07/91
#include "fatmen/fatbug.inc"
#include "fatmen/fattyp.inc"
      CHARACTER*8  PTYP,GTYP,SIZE,CDEN,CMNT,CLAB
      CHARACTER*80 COMAND
#include "fatmen/fatsys.inc"
#include "fatmen/fatinit.inc"
 
      CALL KUGETI(NMEDIA)
      CALL KUGETC(PTYP,LT)
      CALL KUGETC(GTYP,LG)
      CALL KUGETC(SIZE,LS)
      CALL KUGETC(CDEN,LD)
      CALL KUGETC(CMNT,LM)
      CALL KUGETC(CLAB,LL)
      CALL KUPATL(COMAND,NPARS)
 
 
      IF(NPARS.EQ.0) THEN
         ISTART = 1
         IEND   = NMTYP
      ELSE
         ISTART = NMEDIA
         IEND   = NMEDIA
      ENDIF
 
      IF(NPARS.LT.2) THEN
 
         DO 10 I=ISTART,IEND
            IF(MEDSIZ(I).LT.0) GOTO 10
            LT = LENOCC(CHMTYP(I))
            LG = LENOCC(CHMGEN(I))
            LS = LENOCC(CHMSIZ(I))
            LD = LENOCC(CHMDEN(I))
            LM = LENOCC(CHMMNT(I))
            LL = LENOCC(CHMLAB(I))
            PRINT 9001,I,CHMTYP(I)(1:LT),CHMGEN(I)(1:LG),
     +                   CHMSIZ(I)(1:LS),CHMDEN(I)(1:LD),
     +                   CHMMNT(I)(1:LM),CHMLAB(I)(1:LL)
 9001       FORMAT(' FMSMDK. Media type: ',I3,' device: ',A,
     +             ' generic device: ',A,/,
     +             '        capacity: ',A,' density: ',A,/,
     +             '        mount type: ',A,' label type: ',A)
   10    CONTINUE
 
         RETURN
 
      ELSE
 
         J      =  NMEDIA
 
         IF(J.GT.NMTYP) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMSMDK. media type ',J,
     +         ' outside range. Maximum allowed value = ',NMTYP
            RETURN
         ENDIF
 
         IF(IDEBFA.GE.1) THEN
            PRINT *,'FMSMDK. setting following attributes:'
            PRINT 9001,NMEDIA,PTYP(1:LT),GTYP(1:LG), SIZE(1:LS),CDEN(1:
     +      LD), CMNT(1:LM),CLAB(1:LL)
         ENDIF
*
*     Physical device type
*
         CHMTYP(J) = PTYP(1:LT)
*
*     Generic device type
*
         CHMGEN(J) = GTYP(1:LG)
*
*     Capacity
*
         CHMSIZ(J) = SIZE(1:LS)
         MEDSIZ(J) = ICDECI(CHMSIZ(J),1,LS)
*
*     Density
*
         CHMDEN(J) = CDEN(1:LD)
         MEDDEN(J) = ICDECI(CDEN,1,LD)
         IF(INDEX(CHMDEN(J),'K').NE.0) MEDDEN(J) = MEDDEN(J) * 1000
*
*     Label type
*
         CHMLAB(J) = CLAB(1:LL)
*
*     Mount type
*
         CHMMNT(J) = CMNT(1:LM)
*
      ENDIF
 
      END
