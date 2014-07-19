*
* $Id: hxnorm.F,v 1.1.1.1 1996/01/16 17:07:49 mclareni Exp $
*
* $Log: hxnorm.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:49  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HXNORM (X)
*.==========>
*.      NORMALIZATION OF THE X-SPACE BY LINEAR TRANSFORMATION
*..=========> ( R.Brun )
#include "hbook/hcpar1.inc"
#include "hbook/hcpar2.inc"
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION COEFF
#endif
      DIMENSION X(NPMAX,ND)
*
*  FIND VARIABLE RANGE
*
      DO 10 J=1,ND
         XMIN(J)=X(1,J)
         XMAX(J)=XMIN(J)
         DO 5 I=2,NP
            IF (X(I,J).LT.XMIN(J)) XMIN(J)=X(I,J)
            IF (X(I,J).GT.XMAX(J)) XMAX(J)=X(I,J)
    5    CONTINUE
   10 CONTINUE
*
*  PERFORM NORMALIZATION [0,-->[, [0,1] OR [-1,1]
*
      IF (IOPT(8).EQ.3) THEN
         DO 20 J=1,ND
            XMN=XMIN(J)
            DO 15 I=1,NP
               X(I,J)=X(I,J)-XMN
   15       CONTINUE
   20    CONTINUE
      ELSE
         DO 25 I=1,ND
            IF (IOPT(8).EQ.1) THEN
               ALIM(I)=-1.
               BLIM(I)=1.
            ELSE IF (IOPT(8).EQ.2) THEN
               ALIM(I)=0.
               BLIM(I)=1.
            ENDIF
   25    CONTINUE
         DO 35 J=1,ND
            AL=ALIM(J)
            BAL=BLIM(J)-AL
            XMN=XMIN(J)
            XR=XMAX(J)-XMN
            DO 30 I=1,NP
               X(I,J)=AL+BAL*(X(I,J)-XMN)/XR
   30       CONTINUE
   35    CONTINUE
      ENDIF
      END
