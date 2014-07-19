*
* $Id: hrval.F,v 1.1.1.1 1996/01/16 17:07:47 mclareni Exp $
*
* $Log: hrval.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:47  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.53  by  Rene Brun
*-- Author :
#if !defined(CERNLIB_DOUBLE)
      FUNCTION HRVAL (X)
#endif
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION FUNCTION HRVAL (X)
#endif
*.==========>
*.      GIVES THE VALUE OF THE REGRESSION COMPUTED AT
*.      POINT X (PREDICTION)
*..=========> ( D.Lienart )
      DIMENSION X(1),XN(10)
#include "hbook/hcpar1.inc"
#include "hbook/hcpar2.inc"
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION COEFF,P,HSTELF,HELEFT,HBASFT
#endif
*
*  NORMALIZE X USING THE SAME TRANSFORMATION AS FOR THE
*  ORIGINAL SAMPLE
*
      IF (IOPT(8).EQ.0) THEN
         DO 5 I=1,ND
            XN(I)=X(I)
    5    CONTINUE
      ELSE
         IF (IOPT(8).EQ.3) THEN
            DO 10 I=1,ND
               XN(I)=X(I)-XMIN(I)
   10       CONTINUE
         ELSE
            DO 15 I=1,ND
               XN(I)=ALIM(I)+(BLIM(I)-ALIM(I))*(X(I)-XMIN(I))/
     +         (XMAX(I)-XMIN(I))
   15       CONTINUE
         ENDIF
      ENDIF
*
*  COMPUTE REGRESSION VALUE
*
      HRVAL=0.
      DO 30 K=1,NCO
         P=1.
         DO 20 I=1,ND
            NUM=IBASFT(I,K)/10
            ITYP=IBASFT(I,K)-NUM*10
            IF (NUM.NE.0) THEN
               IF (ITYP.EQ.0) P=P*HSTELF(IOPT(4),NUM,XN(I))
               IF (ITYP.EQ.1) P=P*HELEFT(NUM,XN(I))
               IF (ITYP.EQ.2) THEN
                  P=HBASFT(NUM,XN)
                  GOTO 25
               ENDIF
            ENDIF
   20    CONTINUE
   25    HRVAL=HRVAL+COEFF(K)*P
   30 CONTINUE
      END
