*
* $Id: hpr1v.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hpr1v.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HPR1V(C,E,F,A,W,ICAS,NCX,NUM,XX0,XXSIZE,XMIN,XMAX)
*.==========>
*.           CONTROL ROUTINE TO PRINT A 1-DIM HIST VERTICAL
*..=========> ( R.Brun )
      DIMENSION C(3),E(1),F(1),A(1),W(1)
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
      NC=NCX-2
      ICN=NC
      NHT=NCOLPA-16
      IF(NHT.GT.100)NHT=100
      NHT=(NC+NHT-1)/NHT
      ALLCHA=0.
      ICMAX=0
      XMAXI=-BIGP
      XMINI=-XMAXI
      IF(NHT.NE.1)THEN
         DO 5 I=1,NC
            C(1)=HCX(I,1)
            ALLCHA=ALLCHA+C(1)
            XMAXI=MAX(XMAXI,C(1))
            IF(I26.NE.0.AND.C(1).EQ.0.)GO TO 5
            XMINI=MIN(XMINI,C(1))
    5    CONTINUE
         FACTOR=ALLCHA
         IF(I18.NE.0)THEN
            FACTOR=Q(LCID+KNORM)
            IF(ALLCHA.NE.0.)FACTOR=FACTOR/ALLCHA
         ENDIF
         IF(FACTOR.EQ.ALLCHA)FACTOR=1.
         C(1)=XMAXI*FACTOR
         IF(I20.NE.0)THEN
            XMAX20=Q(LCID+KMAX1)*FACTOR
            IF(XMAX20.GT.C(1))C(1)=XMAX20
         ENDIF
         CALL HFACT(C,1,ISIGNE,IEXP1,IEXP2,FACT)
         XMAXI=C(1)
         XMINI=XMINI*FACTOR*FACT
         IF(I20.NE.0)XMAXI=Q(LCID+KMAX1)*FACT
         IF(I21.NE.0)XMINI=Q(LCID+KMIN1)*FACT
      ENDIF
*
      XMAXX=XMAXI
      XMINX=XMINI
      XINT=0.
*
      DO 100 IH=1,NHT
*
         XMINI=XMINX
         XMAXI=XMAXX
         ICMIN=ICMAX+1
         ICMAX=ICMIN+NCOLPA-29
         IF(ICMAX.GT.NC)ICMAX=NC
*
         J=0
         ICN=ICMAX-ICMIN+1
         IF(I34.NE.0)CALL VZERO(E,ICN)
         IF(I12.NE.0)CALL VZERO(F,ICN)
*
         DO 10 ICX=ICMIN,ICMAX
            J=J+1
            C(J)=HCX(ICX,1)
            IF(I11.NE.0)THEN
               E(J)=SQRT(ABS(C(J)))
            ELSE
               IF(NOENT.NE.0)E(J)=HCX(ICX,2)
            ENDIF
            IF(I12.NE.0)F(J)=HCX(ICX,3)
            IF(NHT.NE.1)THEN
               C(J)=C(J)*FACTOR*FACT
               IF(I34.NE.0)E(J)=E(J)*FACTOR*FACT
               IF(I12.NE.0)F(J)=F(J)*FACT
            ELSE
               ALLCHA=ALLCHA+C(J)
            ENDIF
   10    CONTINUE
         IF(NHT.NE.1)GO TO 25
*
         FACTOR=ALLCHA
         IF(I18.NE.0)THEN
            FACTOR=Q(LCID+KNORM)
            IF(ALLCHA.NE.0.)FACTOR=FACTOR/ALLCHA
         ENDIF
         IF(FACTOR.EQ.ALLCHA)FACTOR=1.
         DO 15 J=1,ICN
            C(J)=C(J)*FACTOR
            IF(I34.NE.0)E(J)=E(J)*FACTOR
   15    CONTINUE
         IF(I20.EQ.0)THEN
            CALL HFACT(C,ICN,ISIGNE,IEXP1,IEXP2,FACT)
         ELSE
            C(ICN+1)=Q(LCID+KMAX1)
            CALL HFACT(C,ICN+1,ISIGNE,IEXP1,IEXP2,FACT)
         ENDIF
         XMAXI=VMAX(C,ICN)
         XMINI=BIGP
         DO 20 J=1,ICN
            IF(I34.NE.0)E(J)=E(J)*FACT
            IF(I12.NE.0)F(J)=F(J)*FACT
            IF(I26.NE.0.AND.C(J).LE.0.)GO TO 20
            XMINI=MIN(XMINI,C(J))
   20    CONTINUE
         IF(I20.NE.0)XMAXI=Q(LCID+KMAX1)*FACT
         IF(I21.NE.0)XMINI=Q(LCID+KMIN1)*FACT
*
*             DEFINITION OF STEP WHEN HBIGBI
*
         IF(I17.NE.0)THEN
            MSTEP=JBYT(IQ(LCID),1,4)
            IF(MSTEP.EQ.0)MSTEP=NCOLMA/ICN
            IF(MSTEP.GT.NCOLMA/ICN)MSTEP=1
            IF(MSTEP.EQ.0)MSTEP=1
            K=0
            DO 22 I=1,ICN
               DO 22 J=1,MSTEP
                  K=K+1
                  A(K)=C(I)
   22       CONTINUE
            CALL UCOPY2(A,C,ICN*MSTEP)
*
            IF(I34.NE.0)THEN
               K=0
               DO 23 I=1,ICN
                  DO 23 J=1,MSTEP
                     K=K+1
                     A(K)=E(I)
   23          CONTINUE
               CALL UCOPY2(A,E,ICN*MSTEP)
            ENDIF
*
            IF(I12.NE.0)THEN
               K=0
               DO 24 I=1,ICN
                  DO 24 J=1,MSTEP
                     K=K+1
                     A(K)=F(I)
   24          CONTINUE
               CALL UCOPY2(A,F,ICN*MSTEP)
            ENDIF
         ENDIF
*
*             PRINT TITLE AND HISTOGRAM
*
   25    ICN=ICN*MSTEP
         NLTIT=1
         IF(LGTIT.NE.0)NLTIT=NLTIT+3
         NLCONT=3*(1-I29)+5*(2+I14+I31+I22-I30-I15)
         NLSTAT=I25*(2+I12)
         NLINE=NLINPA-NLTIT-NLCONT-NLSTAT+NLINPA*I23
*
         CALL HPTIT(ICAS,NUM,XMIN,XMAX)
*
         IF(I16.EQ.0)THEN
            CALL HP1DIM(C,E,F,ICN,XMINI,XMAXI,NLINE)
         ENDIF
         CALL HFORMA(2)
         MST=-1
*
*
*             PRINT CHANNELS
*
         IF(I29.EQ.0)THEN
            NLTIT=IDG(41)
            IF(I17.NE.0)NLTIT=IDG(37)
            NLCONT=ICMAX*MSTEP
            CALL HPCHAN(NLTIT,ICMIN,NLCONT,ICN,A)
            CALL HFORMA(2)
         ENDIF
*
*             PRINT CONTENTS
*
         IF(I30.EQ.0)THEN
            K=0
            DO 35 I=MSTEP,ICN,MSTEP
               K=K+1
               C(K)=C(I)
   35       CONTINUE
            XMAXI=VMAX(C,ICN)
            XMAXI=ABS(XMAXI)
            XMINI=VMIN(C,ICN)
            IF(ABS(XMINI).GT.XMAXI)XMAXI=XMINI
            CALL HPCONT('CONTENTS',C,ICN,1,XMAXI,A,MST+3,W,ISIGNE,
     +      IEXP1,IEXP2)
         ENDIF
*
*             PRINT ERRORS
*
         IF(I31*I34.NE.0)THEN
            K=0
            DO 45 I=MSTEP,ICN,MSTEP
               K=K+1
               E(K)=E(I)
   45       CONTINUE
            XMAXI=VMAX(E,ICN)
            CALL HPCONT('ERROR   ',E,ICN,2,XMAXI,A,MST+3,W,ISIGNE,
     +      IEXP1,IEXP2)
         ENDIF
*
*             PRINT FUNCTION
*
         IF(I14.NE.0.AND.I12.NE.0)THEN
            K=0
            DO 55 I=MSTEP,ICN,MSTEP
               K=K+1
               F(K)=F(I)
   55       CONTINUE
            XMAXI=VMAX(F,ICN)
            XMINI=VMIN(F,ICN)
            IF(ABS(XMINI).GT.XMAXI)XMAXI=XMINI
            CALL HPCONT('FUNCTION',F,ICN,3,XMAXI,A,MST+3,W,ISIGNE,
     +      IEXP1,IEXP2)
         ENDIF
*
*             PRINT INTEGRATED CONTENTS
*
         IF(I22.NE.0)THEN
            CALL VZERO(A,ICN)
            A(1)=XINT+C(1)
            DO 70 I=2,ICN
               A(I)=A(I-1)+C(I)
   70       CONTINUE
            XINT=A(ICN)
            CALL UCOPY2(A,C,ICN)
            CALL HFACT(C,ICN,IL1,IL2,IL3,CHI)
            XMAXI=ABS(VMAX(C,ICN))
            XMINI= VMIN(C,ICN)
            IF(ABS(XMINI).GT.XMAXI) XMAXI=XMINI
            CALL HPCONT('INTEGRAT',C,ICN,4,XMAXI,A,MST+3,W,IL1,IL2,IL3)
         ENDIF
*
*             PRINT LOW-EDGE
*
         IF(I15.EQ.0)THEN
            C(1)=X0
            DO 90 I=2,ICN
               IF(I6.EQ.0)THEN
                  C(I)=C(I-1)+XSIZE
               ELSE
                  LBINS=LQ(LCID-2)
                  C(I)=Q(LBINS+I)
               ENDIF
   90       CONTINUE
            X0=C(ICN)+XSIZE
            CALL HFACT(C,ICN,IL1,IL2,IL3,CHI)
            MST=2
            XMAXI=ABS(C(ICN))
            IF(ABS(C(1)).GT.XMAXI)XMAXI=C(1)
            CALL HPCONT('LOW-EDGE',C,ICN,5,XMAXI,A,MST,W,IL1,IL2,IL3)
         ENDIF
  100 CONTINUE
      IEXP2=IDG(41)
      IEXPL2=IDG(41)
*
*             PRINT STATISTICS
*
      IF(I25.EQ.0)THEN
         ICN=ICMAX
         CHI=-1.
         IF(I12.NE.0)THEN
            CHI=0.
            DO 110 I=1,ICN
               C(1)=HCX(I,1)
               C(2)=HCX(I,3)/FACTOR
               C(3)=C(1)
               IF(NOENT.NE.0)C(3)=HCX(I,2)**2
               IF(C(3).EQ.0.)GO TO 110
               CHI=CHI+((C(1)-C(2))**2)/C(3)
  110       CONTINUE
         ENDIF
*
         CALL HPRST(ALLCHA,ISIGNE,ISIGNL,IEXP1,IEXP2,
     +             IEXPL1,IEXPL2,CHI)
      ENDIF
*
      END
