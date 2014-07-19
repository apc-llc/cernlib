*
* $Id: hpakad.F,v 1.2 1996/01/19 10:16:58 couet Exp $
*
* $Log: hpakad.F,v $
* Revision 1.2  1996/01/19 10:16:58  couet
* The number entries is reset to 0
*
* Revision 1.1.1.1  1996/01/16 17:07:45  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.49.50  by  Julian Bunn
*-- Author :
      SUBROUTINE HPAKAD(IDD,X)
*.==========>
*.           Fill histogram with array X
*.           At the difference of HPAK which overwrites
*.           the current channel contents by the new values in X,
*.           HPAKAD adds the new values to the existing contents.
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
      DIMENSION X(1)
*.___________________________________________
      CALL HFIND(IDD,'HPAK  ')
*
      IF(LCID.EQ.0)GO TO 99
      CALL HDCOFL
      IF(I123.EQ.0)GO TO 99
      CALL SBIT0(IQ(LCID),5)
      NEOLD=IQ(LCONT+KNOENT)
      NCX=IQ(LCID+KNCX)
      IQ(LCONT+KNOENT)=0
      IF(I1.NE.0)THEN
         LPRX=LCID+KNCX
         DO 10 I=1,NCX
            XX=HCX(I,1) + X(I)
            CALL HFCX(I,XX)
  10     CONTINUE
         IF(IQ(LCONT+KNOENT).LT.0)THEN
            IQ(LCONT+KNOENT)=NEOLD+NCX
         ENDIF
      ELSE
*
         NCY=IQ(LCID+KNCY)
         K=0
         DO 30 I=1,NCY
            DO 20 J=1,NCX
               K=K+1
               XX=X(K) + HCXY(J,I,1)
               CALL HFCXY(J,I,XX)
  20        CONTINUE
  30     CONTINUE
         IF(IQ(LCONT+KNOENT).LT.0)THEN
            IQ(LCONT+KNOENT)=NEOLD+NCX*NCY
         ENDIF
      ENDIF
*
  99  RETURN
      END
