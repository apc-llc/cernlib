*
* $Id: hcorrl.F,v 1.1.1.1 1996/01/16 17:07:34 mclareni Exp $
*
* $Log: hcorrl.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:34  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 31/08/90  18.52.55  by  Rene Brun
*-- Author :
      SUBROUTINE HCORRL (X,Y)
*.==========>
*.      BUILDS CORRELATION MATRIX OF X WITH Y AND
*.      X WITH X -  OUTPUT ON UNIT LOUT
*..=========> ( D.Lienart )
#include "hbook/hcpar1.inc"
#include "hbook/hcunit.inc"
      DIMENSION X(NPMAX,ND),Y(1),C(11,10),XM(10)
*
* COMPUTED XM MEAN VALUE OF X FOR EACH DIMENSION
*
      DO 10 I=1,ND
         XM(I)=0.
         DO 5 K=1,NP
            XM(I)=XM(I)+X(K,I)
    5    CONTINUE
         XM(I)=XM(I)/NP
   10 CONTINUE
*
*  COMPUTE YM MEAN VALUE OF Y
*
      YM=0.
      DO 15 K=1,NP
         YM=YM+Y(K)
   15 CONTINUE
      YM=YM/NP
*
*  COMPUTE CORRELATIONS BETWEEN X AND Y
*
      DO 20 I=1,ND
         C(1,I)=-NP*XM(I)*YM
         DO 20 K=1,NP
            C(1,I)=C(1,I)+X(K,I)*Y(K)
   20 CONTINUE
*
*  COMPUTE AUTOCORRELATIONS OF X
*
      DO 25 I=1,ND
         DO 25 J=1,I
            C(I+1,J)=-NP*XM(I)*XM(J)
            DO 25 K=1,NP
               C(I+1,J)=C(I+1,J)+X(K,I)*X(K,J)
   25 CONTINUE
*
*  PRINT MATRIX C
*
      WRITE (LOUT,100)
      WRITE (LOUT,200) (C(1,J),J=1,ND)
      DO 30 I=1,ND
         WRITE (LOUT,200) (C(I+1,J),J=1,I)
   30 CONTINUE
  100 FORMAT (//' X-Y AND X-X CORRELATIONS'/,1X,24('-'))
  200 FORMAT (1X,10(G12.5,1X))
      END
