*
* $Id: hf2.F,v 1.1.1.1 1996/01/16 17:07:36 mclareni Exp $
*
* $Log: hf2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/09 25/07/94  16.36.31  by  Rene Brun
*-- Author :
      SUBROUTINE HF2(ID1,X,Y,W)
*.==========>
*.           FAST FILLING ENTRY FOR A 2-DIM HISTOGRAM
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcprin.inc"
*.___________________________________________
#include "hbook/jbyt.inc"
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
*
*              Errors
*
      LW=LQ(LCONT)
      IF(LW.NE.0)THEN
         IF(I.GT.0.AND.I.LE.IQ(LCID+KNCX).AND.J.GT.0.AND.
     +                 J.LE.IQ(LCID+KNCY))THEN
            K=IQ(LCID+KNCX)*(J-1)+I
            Q(LW+K)=Q(LW+K)+W*W
         ENDIF
      ENDIF
*
*              Contents
*
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
