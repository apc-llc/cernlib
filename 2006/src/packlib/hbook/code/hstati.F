*
* $Id: hstati.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hstati.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  18.56.16  by  Rene Brun
*-- Author :
      FUNCTION HSTATI(IDD,IS,KCASE,NUMM)
*.==========>
*.           IS=1   MEAN IS RETURNED
*.           IS=2   R.M.S.
*.           IS>2   NUMBER OF EQUIVALENT EVENTS
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcfitr.inc"
      DOUBLE PRECISION SWX(2),XVAL(2),VALMEA,RMS,X,W
      CHARACTER*(*) KCASE
*.___________________________________________
      HSTATI=0.
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
         CALL HFINOP(IDD,'HSTATI',IFW,NB,IFX,IFY,ICAS,NUM)
         LCONT=IFW
      ELSE
         IFW=LCONT
         IFX=LPRX
         IFY=LPRY
      ENDIF
*
      IF(IFW.EQ.0)GO TO 99
      IF(IFY.NE.0)GO TO 99
      NCX=IQ(IFX)
      BWID=(Q(IFX+2)-Q(IFX+1))/FLOAT(NCX)
      XVAL(1)=0.
      XVAL(2)=0.
      X=Q(IFX+1)-0.5*BWID
      IF(IFTRNG.NE.0)THEN
         I7=0
         IC1=IFXLOW
         IC2=IFXUP
      ELSE
         I7=JBIT(IQ(LCID+KBITS),7)
         IC1=1
         IC2=NCX
      ENDIF
      IF(I7.NE.0)THEN
         ALLCHA=Q(IFW+KSTAT1)
      ELSE
         ALLCHA=0.
         DO 10 I=1,NCX
            X=X+BWID
            IF(I.LT.IC1.OR.I.GT.IC2)GO TO 10
            W=ABS(HCX(I,1))
            ALLCHA=ALLCHA+W
            XVAL(1)=XVAL(1)+W*X
            XVAL(2)=XVAL(2)+W*X*X
  10     CONTINUE
      ENDIF
*
      IF(ALLCHA.EQ.0.)GO TO 99
      IF(IS.GE.3)THEN
         HSTATI=IQ(IFW+KNOENT)
         IF(I7.NE.0)THEN
            EQUIV=Q(IFW+KSTAT1+1)
            IF(EQUIV.NE.0.)HSTATI=ALLCHA*ALLCHA/EQUIV
         ENDIF
         GO TO 99
      ENDIF
*
      IF(I7.NE.0)THEN
         CALL UCOPY(Q(IFW+KSTAT1+2),SWX,4)
         XVAL(1)=SWX(1)
         XVAL(2)=SWX(2)
         RMS=SQRT(ABS(SWX(2)/ALLCHA - (SWX(1)/ALLCHA)**2))
      ENDIF
*
      VALMEA=XVAL(1)/ALLCHA
      IF(IS.EQ.1)THEN
         HSTATI=VALMEA
         GO TO 99
      ENDIF
*
      IF(I7.EQ.0)THEN
         RMS=XVAL(2)/ALLCHA - VALMEA*VALMEA
         RMS=SQRT(ABS(RMS))
      ENDIF
      IF(RMS.EQ.0.)GO TO 99
      IF(IS.LE.2)THEN
         HSTATI=RMS
         GO TO 99
      ENDIF
*
      HSTATI=0.
*
  99  CONTINUE
      END
