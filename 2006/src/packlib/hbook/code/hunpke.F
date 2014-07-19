*
* $Id: hunpke.F,v 1.1.1.1 1996/01/16 17:07:49 mclareni Exp $
*
* $Log: hunpke.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:49  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.57.00  by  Julian Bunn
*-- Author :
      SUBROUTINE HUNPKE(IDD,X,CHCASE,NUMM)
*.==========>
*.           This routine unpacks errors corresponding to hist ID
*.           following values of KCASE, HUNPKE returns the errors of
*.           HISTogram, or PROJection, SLI(X/Y)ces or BAN(X/Y)ds
*..=========> ( R. Namjoshi, modified from HUNPAK )
#include "hbook/hcbook.inc"
      CHARACTER*(*) CHCASE
      CHARACTER*4 KCASE
      DIMENSION X(1)
*.___________________________________________
      ICAS = 0
      NUM  = NUMM
      KCASE=CHCASE
      IF(KCASE.EQ.' ')KCASE='HIST'
      CALL UCTOH(KCASE,ICAS,4,4)
*
      IF( LFIX.EQ.0 ) THEN
         CALL HFINOP( IDD, 'HUNPKE', IFW, NB, IFX, IFY, ICAS, NUM )
         LCONT=LQ(LCID-1)
      ELSE
         IFW = LCONT
         IFX = LPRX
         IFY = LPRY
      ENDIF
*
      IF( IFW.EQ.0 ) THEN
         CALL HBUG( 'Cannot find projection', 'HUNPKE', IDD )
         GO TO 99
      ENDIF
      LCONT = IFW
      NCX   = IQ(IFX)
      IF( IFY.EQ.0 ) THEN
         IHBX = JBIT(IQ(LCID+KBITS),9)
         IHBY = JBIT(IQ(LCID+KBITS),10)
         ISQR = 1
         IF( (KCASE(4:4).EQ.'X'   .AND. IHBX.NE.0) .OR.
     &       (KCASE(1:4).EQ.'HIST'.AND. IHBX.NE.0) .OR.
     &       (KCASE(4:4).EQ.'Y'   .AND. IHBY.NE.0)      ) ISQR = 0
         DO 10 I = 1, NCX
            IF( ISQR.EQ.0 ) THEN
               X(I) = HCX(I,2)
            ELSE
               RES  = ABS(HCX(I,1))
               X(I) = SQRT(RES)
            ENDIF
  10     CONTINUE
         GO TO 99
      ENDIF
*
  20  NCY   = IQ(IFY)
      K     = 0
      DO 35 J = 1, NCY
         DO 30 I = 1, NCX
            K = K + 1
            X(K) = HCXY(I,J,2)
  30     CONTINUE
  35  CONTINUE
*
  99  RETURN
      END
