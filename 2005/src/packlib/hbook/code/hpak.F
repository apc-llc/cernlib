*
* $Id: hpak.F,v 1.1.1.1 1996/01/16 17:07:45 mclareni Exp $
*
* $Log: hpak.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:45  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/02 05/05/94  09.30.22  by  Rene Brun
*-- Author :
      SUBROUTINE HPAK(IDD,X)
*.==========>
*.           Fill histogram with array X
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
      DIMENSION X(1)
*.___________________________________________
      CALL HFIND(IDD,'HPAK  ')
*
      IF(LCID.EQ.0)GO TO 99
      CALL HDCOFL
      IF(I123.EQ.0)GO TO 40
      CALL SBIT0(IQ(LCID),5)
      NCX=IQ(LCID+KNCX)
      IQ(LCONT+KNOENT)=0
      IF(I1.NE.0)THEN
         NW=IQ(LCONT-1)-KCON1+1
         CALL VZERO(Q(LCONT+KCON1),NW)
         I7=JBIT(IQ(LCID+KBITS),7)
         IF(I7.NE.0)CALL VZERO(Q(LCONT+KSTAT1),6)
         LPRX=LCID+KNCX
         DO 10 I=1,NCX
            XX=FLOARG(X(I))
            CALL HFCX(I,XX)
  10     CONTINUE
         IF(IQ(LCONT+KNOENT).LE.0)THEN
            IQ(LCONT+KNOENT)=NCX
         ENDIF
      ELSE
*
         NW=IQ(LCONT-1)-KCON2+1
         CALL VZERO(Q(LCONT+KCON2),NW)
         NCY=IQ(LCID+KNCY)
         K=0
         DO 30 I=1,NCY
            DO 20 J=1,NCX
               K=K+1
               XX=FLOARG(X(K))
               CALL HFCXY(J,I,XX)
  20        CONTINUE
  30     CONTINUE
         IF(IQ(LCONT+KNOENT).LE.0)THEN
            IQ(LCONT+KNOENT)=NCX*NCY
         ENDIF
      ENDIF
      GO TO 99
*
  40  NW=IQ(LCID-1)
      CALL UCOPY(X,Q(LCID+1),NW)
*
  99  RETURN
      END
