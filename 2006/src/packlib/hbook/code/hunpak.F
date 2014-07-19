*
* $Id: hunpak.F,v 1.1.1.1 1996/01/16 17:07:49 mclareni Exp $
*
* $Log: hunpak.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:49  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.53.13  by  Julian Bunn
*-- Author :
      SUBROUTINE HUNPAK(IDD,X,KCASE,NUMM)
*.==========>
*.           THIS ROUTINE UNPACKS VALUES CORRESPONDING TO HIST ID
*.           FOLLOWING VALUES OF KCASE HUNPAK RETURNS THE CONTENTS OF
*.           HISTOGRAM,OR PROJECTION,SLICES OR BANDES
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
      DIMENSION X(1)
      CHARACTER*(*) KCASE
*.___________________________________________
      NARG=4
      CALL NOARG(NARG)
      ICAS=0
      NUM=0
      IF(NARG.EQ.4)NUM=NUMM
      IF(NARG.GT.2)THEN
         CALL UCTOH(KCASE,ICAS,4,4)
      ENDIF
*
      IF(LFIX.EQ.0)THEN
         CALL HFINOP(IDD,'HUNPAK',IFW,NB,IFX,IFY,ICAS,NUM)
         LCONT=LQ(LCID-1)
      ELSE
         IFW=LCONT
         IFX=LPRX
         IFY=LPRY
      ENDIF
*
      IF(IFW.EQ.0)THEN
         IF(JBYT(IQ(LCID+KBITS),1,3).EQ.0)GO TO 40
         CALL HBUG('Cannot find projection','HUNPAK',IDD)
         GO TO 99
      ENDIF
      LCONT=IFW
      NCX=IQ(IFX)
      IF(IFY.EQ.0)THEN
         DO 10 I=1,NCX
            X(I)=HCX(I,1)
  10     CONTINUE
         GO TO 99
      ENDIF
*
      NCY=IQ(IFY)
      LSCAT=LCONT
      K=0
      DO 30 J=1,NCY
         DO 20 I=1,NCX
            K=K+1
            X(K)=HCXY(I,J,1)
  20     CONTINUE
  30  CONTINUE
      GO TO 99
*
  40  NW=IQ(LCID-1)
      CALL UCOPY(Q(LCID+1),X,NW)
*
  99  RETURN
      END
