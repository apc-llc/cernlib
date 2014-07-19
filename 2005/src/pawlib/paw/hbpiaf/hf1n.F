*
* $Id: hf1n.F,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
*
* $Log: hf1n.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 22/08/93  11.13.19  by  Fons Rademakers
*-- Author :
      SUBROUTINE HF1N(ID1,X,W,N)
*.==========>
*.           Fill 1-Dim histogram with N values
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
      DOUBLE PRECISION SWX(2)
      DIMENSION X(1),W(1)
      SAVE XMIN,XMAX,NCHAN,BWID1
*.___________________________________________
*
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
         XMIN=Q(LPRX+1)
         XMAX=Q(LPRX+2)
         NCHAN=IQ(LPRX)
         BWID1=FLOAT(NCHAN)/(XMAX-XMIN)
      ENDIF
      CALL SBIT0(IQ(LCID),6)
*
      IF(I5.NE.0)THEN
         DO 10 I=1,N
            CALL HF1(ID1,X(I),W(I))
   10    CONTINUE
         RETURN
      ENDIF
      IQ(LCONT+KNOENT)=IQ(LCONT+KNOENT)+N
      NBPROX=IQ(LCONT+KNBIT)
      DO 30 I=1,N
         IF(X(I).LT.XMIN)THEN
            ICHAN=0
         ELSEIF(X(I).GE.XMAX)THEN
            ICHAN=NCHAN+1
         ELSE
            IF(I6.EQ.0)THEN
               ICHAN=(X(I)-XMIN)*BWID1 + 1
            ELSE
               LBINS=LQ(LCID-2)
               ICHAN=1
   20          IF(X(I).GE.Q(LBINS+ICHAN+1))THEN
                  ICHAN=ICHAN+1
                  GO TO 20
               ENDIF
            ENDIF
         ENDIF
*
         IF(NBPROX.GE.32)THEN
            Q(LCONT+ICHAN+KCON1)=Q(LCONT+ICHAN+KCON1)+W(I)
         ELSE
            NB=32/NBPROX
            LWORD=ICHAN/NB
            LBIT=(ICHAN-NB*LWORD)*NBPROX+1
            INC=JBYT(IQ(LCONT+LWORD+KCON1),LBIT,NBPROX)
            INC=INC+W(I)+0.5
            IF(INC.GT.MAXBIT(NBPROX))INC=MAXBIT(NBPROX)
            CALL SBYT(INC,IQ(LCONT+LWORD+KCON1),LBIT,NBPROX)
         ENDIF
*
*              ERRORS
*
         IF(LQ(LCONT).NE.0)THEN
            IF(ICHAN.GT.0.AND.ICHAN.LE.NCHAN)THEN
               LW=LQ(LCONT)
               Q(LW+ICHAN)=Q(LW+ICHAN)+W(I)*W(I)
            ENDIF
         ENDIF
*
*              STATISTICS
*
         IF(I7.NE.0)THEN
            IF(ICHAN.GT.0.AND.ICHAN.LE.NCHAN)THEN
               Z=ABS(W(I))
               Q(LCONT+KSTAT1)=Q(LCONT+KSTAT1)+Z
               Q(LCONT+KSTAT1+1)=Q(LCONT+KSTAT1+1)+Z*Z
               CALL UCOPY(Q(LCONT+KSTAT1+2),SWX,4)
               SWX(1)=SWX(1)+Z*X(I)
               SWX(2)=SWX(2)+Z*X(I)*X(I)
               CALL UCOPY(SWX,Q(LCONT+KSTAT1+2),4)
            ENDIF
         ENDIF
   30 CONTINUE
      END
