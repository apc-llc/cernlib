*
* $Id: hpr1h.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hpr1h.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  12.38.31  by  Rene Brun
*-- Author :
      SUBROUTINE HPR1H(ICAS,NC,NUM,XX0,XXSIZE,XMIN,XMAX,AA,BB)
*.==========>
*.           CONTROL ROUTINE TO PRINT A 1-DIM HIST HORIZONTHAL
*..=========> ( R.Brun )
      DIMENSION C(2),E(1),F(1),AA(1)
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
*.___________________________________________
*             NO ENTRIES CASE
*
      IF(IQ(LCONT+KNOENT).EQ.0)THEN
         NOENT=2
         NH=NH+1
         CALL HPTIT(ICAS,NUM,0.,0.)
         RETURN
      ENDIF
*
      IF(I1.EQ.0)THEN
         I20=0
         I21=0
      ENDIF
      NOLD=4
      I34=NOENT
      NH=NH+1
      IF(I11.NE.0)I34=1
      X0=XX0
      XSIZE=XXSIZE
      MSTEP=1
      NHT=1
      IH=1
      ICN=NC
*
*             ROTATE HISTOGRAM by 90 DEGREES
*
      MSTEP=1
      IF(I17.NE.0)THEN
         MSTEP=JBYT(IQ(LCID),1,4)
         IF(MSTEP.EQ.0)MSTEP=5
      ENDIF
      XMINI=X0
      XMAXI=X0+FLOAT(NC-1)*XSIZE
      IF(ABS(XMINI).GT.ABS(XMAXI))XMAXI=XMINI
      C(1)=XMAXI
      CALL HFACT(C(1),1,ISIGNL,IEXPL1,IEXPL2,FACT)
      X0=X0*FACT
      XSIZE=XSIZE*FACT
      FACTL=FACT
      CALL HPTIT(ICAS,NUM,XMIN,XMAX)
      CALL HFORMA(2)
*
      XMAXI=-BIGP
      XMINI=-XMAXI
      ALLCHA=0.
      DO 120 ICX=1,NC
         C(1)=HCX(ICX,1)
         XMAXI=MAX(XMAXI,C(1))
         IF(I26.NE.0.AND.C(1).EQ.0.)GO TO 120
         XMINI=MIN(XMINI,C(1))
         ALLCHA=ALLCHA+C(1)
 120  CONTINUE
      IEXP2=IDG(41)
*
      IF(I20.NE.0)XMAXI=Q(LCID+KMAX1)
      IF(I21.NE.0)THEN
         IF(I26.NE.0.)THEN
            IF(Q(LCID+KMIN1).GT.0.)XMINI=Q(LCID+KMIN1)
         ENDIF
      ENDIF
      IF(XMINI.GE.XMAXI)GO TO 150
      FACTOR=ALLCHA
      IF(I18.NE.0)FACTOR=Q(LCID+KNORM)
      IF(ALLCHA.NE.0.)FACTOR=FACTOR/ALLCHA
      IF(FACTOR.EQ.0.)FACTOR=1.
      XMAXI=XMAXI*FACTOR
      XMINI=XMINI*FACTOR
      C(1)=XMAXI
      IF(ABS(XMINI).GT.ABS(C(1)))C(1)=XMINI
      CALL HFACT(C(1),1,ISIGNE,IEXP1,IEXP2,FACT)
      ICMIN=-1
      XMAXI=XMAXI*FACT
      XMINI=XMINI*FACT
*
      C(2)=XMAXI
      IF(I20.EQ.0)THEN
         IF(I12.NE.0)THEN
            C(2)=HCX(1,3)*FACT
            DO 122 ICX=2,NC
               F(1)=HCX(ICX,3)*FACT
               C(2)=MAX(C(2),F(1))
 122        CONTINUE
         ENDIF
         IF(I34.NE.0.AND.XMAXI.GT.0.)XMAXI=XMAXI+SQRT(XMAXI)
         XMAXI=MAX(C(2),XMAXI)
      ENDIF
      IF(I16.EQ.0)THEN
         DO 145 ICX=1,NC
            DO 140 I=1,MSTEP
               IF(ICX.GT.1.OR.I.NE.1)ICMIN=0
               IF(ICX.EQ.NC.AND.I.EQ.MSTEP)ICMIN=1
               IF(I.EQ.1)THEN
                  C(1)=HCX(ICX,1)*FACTOR*FACT
                  IF(I11.NE.0) E(1)=SQRT(ABS(C(1)*FACTOR*FACT))
                  IF(NOENT.NE.0)E(1)=HCX(ICX,2)*FACTOR*FACT
                  IF(I12.NE.0)F(1)=HCX(ICX,3)*FACT
                  IF(ICX.NE.1)THEN
                     IF(I6.NE.0)THEN
                        LBINS=LQ(LCID-2)
                        X0=FACTL*Q(LBINS+ICX-1)
                        XSIZE=FACTL*(Q(LBINS+ICX))-X0
                     ELSE
                        XSIZE=FACTL*XXSIZE
                        X0=FACTL*(XX0+(ICX-2)*XXSIZE)
                     ENDIF
                  ENDIF
               ENDIF
               CALL HP1ROT(C(1),E(1),F(1),ICMIN,XMINI,XMAXI,X0,XSIZE
     +         ,ISIGNE,IEXP1,IEXP2,AA,BB)
 140        CONTINUE
 145     CONTINUE
      ENDIF
*
*             PRINTING OF STATISTICS
*
 150  IF(I25.EQ.0)THEN
         CHI=-1.
         IF(I12.NE.0)THEN
            CHI=0.
            DO 160 I=1,ICN
               C(1)=HCX(I,1)
               F(1)=HCX(I,3)/FACTOR
               E(1)=C(1)
               IF(NOENT.NE.0)E(1)=HCX(I,2)**2
               IF(E(1).EQ.0.)GO TO 160
               CHI=CHI+((C(1)-F(1))**2)/E(1)
 160        CONTINUE
         ENDIF
*
         CALL HPRST(ALLCHA,ISIGNE,ISIGNL,IEXP1,IEXP2,
     +             IEXPL1,IEXPL2,CHI)
      ENDIF
*
      END
