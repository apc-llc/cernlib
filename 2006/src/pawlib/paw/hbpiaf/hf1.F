*
* $Id: hf1.F,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
*
* $Log: hf1.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 22/08/93  11.13.19  by  Fons Rademakers
*-- Author :
      SUBROUTINE HF1(ID1,X,W)
*.==========>
*.           FAST FILLING ENTRY FOR A 1-DIM HISTOGRAM
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
      DOUBLE PRECISION SWX(2)
*.___________________________________________
#include "hbook/jbyt.inc"
      IF(ID1.NE.IDLAST)THEN
         ID=ID1
***         IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         CALL HIDPOS(ID,IDPOS)
         IF(IDPOS.LE.0)RETURN
         IDLAST=ID1
         LCID=LQ(LTAB-IDPOS)
         LCONT=LQ(LCID-1)
         LPRX=LCID+KNCX
         I5=JBIT(IQ(LCID+KBITS),5)
         I6=JBIT(IQ(LCID+KBITS),6)
         I7=JBIT(IQ(LCID+KBITS),7)
      ENDIF
      CALL SBIT0(IQ(LCID),6)
*
      IF(I5.NE.0)CALL HF1AUT(X,W)
      IQ(LCONT+KNOENT)=IQ(LCONT+KNOENT)+1
      IF(X.LT.Q(LPRX+1))THEN
         ICHAN=0
      ELSEIF(X.GE.Q(LPRX+2))THEN
         ICHAN=IQ(LPRX)+1
      ELSE
         IF(I6.EQ.0)THEN
            ICHAN=(X-Q(LPRX+1))*FLOAT(IQ(LPRX))/(Q(LPRX+2)-Q(LPRX+1))+ 1
         ELSE
            LBINS=LQ(LCID-2)
            ICHAN=1
  10        IF(X.GE.Q(LBINS+ICHAN+1))THEN
               ICHAN=ICHAN+1
               GO TO 10
            ENDIF
         ENDIF
      ENDIF
*
      IF(IQ(LCONT+KNBIT).GE.32)THEN
         Q(LCONT+ICHAN+KCON1)=Q(LCONT+ICHAN+KCON1)+W
      ELSE
         NBPROX=IQ(LCONT+KNBIT)
         NB=32/NBPROX
         LWORD=ICHAN/NB
         LBIT=(ICHAN-NB*LWORD)*NBPROX+1
         INC=JBYT(IQ(LCONT+LWORD+KCON1),LBIT,NBPROX)
         INC=INC+W+0.5
         IF(INC.GT.MAXBIT(NBPROX))INC=MAXBIT(NBPROX)
         CALL SBYT(INC,IQ(LCONT+LWORD+KCON1),LBIT,NBPROX)
      ENDIF
*
*              ERRORS
*
      IF(LQ(LCONT).NE.0)THEN
         IF(ICHAN.GT.0.AND.ICHAN.LE.IQ(LPRX))THEN
            LW=LQ(LCONT)
            Q(LW+ICHAN)=Q(LW+ICHAN)+W*W
         ENDIF
      ENDIF
*
*              STATISTICS
*
      IF(I7.NE.0)THEN
         IF(ICHAN.GT.0.AND.ICHAN.LE.IQ(LPRX))THEN
            Z=ABS(W)
            Q(LCONT+KSTAT1)=Q(LCONT+KSTAT1)+Z
            Q(LCONT+KSTAT1+1)=Q(LCONT+KSTAT1+1)+Z*Z
            CALL UCOPY(Q(LCONT+KSTAT1+2),SWX,4)
            SWX(1)=SWX(1)+Z*X
            SWX(2)=SWX(2)+Z*X*X
            CALL UCOPY(SWX,Q(LCONT+KSTAT1+2),4)
         ENDIF
      ENDIF
      END
