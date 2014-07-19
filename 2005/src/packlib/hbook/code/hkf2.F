*
* $Id: hkf2.F,v 1.1.1.1 1996/01/16 17:07:40 mclareni Exp $
*
* $Log: hkf2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:40  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/08 14/02/94  17.29.20  by  Rene Brun
*-- Author :    Rene Brun   14/02/94
      SUBROUTINE HKF2(LINE,ID1,X,Y,W)
*.==========>
*.           Special fast filling routine for COMIS.
*.           Slow calls to HF1 are replaced by this routine.
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcprin.inc"
*.___________________________________________
      IF(ID1.NE.IDLAST)THEN
         ID=ID1
         IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IF(IDPOS.LE.0)RETURN
         IDLAST=ID1
         LCID=LQ(LTAB-IDPOS)
         LCONT=LQ(LCID-1)
      ENDIF
*
      IQ(LCONT+KNOENT)=IQ(LCONT+KNOENT)+1
      IF(X.LT.Q(LCID+KXMIN))THEN
         I=0
      ELSEIF(.NOT.(X.LT.Q(LCID+KXMAX)))THEN
         I=IQ(LCID+KNCX)+1
      ELSE
         I=(X-Q(LCID+KXMIN))*FLOAT(IQ(LCID+KNCX))/
     +     (Q(LCID+KXMAX)-Q(LCID+KXMIN)) + 1
      ENDIF
*
      IF(Y.LT.Q(LCID+KYMIN))THEN
         J=0
      ELSEIF(.NOT.(Y.LT.Q(LCID+KYMAX)))THEN
         J=IQ(LCID+KNCY)+1
      ELSE
         J=(Y-Q(LCID+KYMIN))*FLOAT(IQ(LCID+KNCY))/
     +     (Q(LCID+KYMAX)-Q(LCID+KYMIN)) + 1
      ENDIF
*
      ICHAN=(IQ(LCID+KNCY)-J+1)*(IQ(LCID+KNCX)+2)+I
      IF(IQ(LCONT+KNBIT).LT.32)THEN
         NBSCAT=IQ(LCONT+KNBIT)
         NB=32/NBSCAT
         LWORD=ICHAN/NB
         LBIT=(NB-1-MOD(ICHAN,NB))*NBSCAT + 1
         IAD=LWORD+LCONT+KCON2
         INC=JBYT(IQ(IAD),LBIT,NBSCAT)+W+0.5
         IF(INC.GT.MAXBIT(NBSCAT))INC=MAXBIT(NBSCAT)
         CALL SBYT(INC,IQ(IAD),LBIT,NBSCAT)
      ELSE
         LWORD=LCONT+ICHAN+KCON2
         Q(LWORD)=Q(LWORD)+W
      ENDIF
      END
