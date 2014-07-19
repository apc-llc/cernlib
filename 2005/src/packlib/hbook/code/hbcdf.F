*
* $Id: hbcdf.F,v 1.1.1.1 1996/01/16 17:07:31 mclareni Exp $
*
* $Log: hbcdf.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:31  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.45  by  Rene Brun
*-- Author :
      SUBROUTINE HBCDF(Y,N,IA)
*.==========>
*.           this routine converts a floating point number in BCD
*.            number of N characters including signe
*..=========> ( R.Brun )
      DIMENSION IA(2)
#include "hbook/hcprin.inc"
*.___________________________________________
      IA(1)=IDG(41)
      IA(2)=IDG(41)
      IA(N-3)=IDG(41)
      IF(Y.LT.-0.001)IA(1)=IDG(39)
      NN=N-2
      M=1000.*ABS(Y)+0.5
      IPOINT=0
      N1=1
      K1=0
      J=N+1
      DO 60 I=1,NN
         J=J-1
         IF(M.LT.N1.AND.I.GT.3)GO TO 40
         N2=10*N1
         K2=MOD(M,N2)
         K=(K2-K1)/N1
         IF(I-4)10,20,30
  10     IF(IPOINT.NE.0)GO TO 15
         IF(K.EQ.0)GO TO 40
         IPOINT=1
  15     IA(J)=IDG(K+1)
         GO TO 50
  20     J=J-1
  30     IA(J)=IDG(K+1)
         GO TO 50
  40     IA(J)=IDG(41)
  50     K1=K2
         N1=N2
  60  CONTINUE
      IF(IPOINT.NE.0)IA(N-3)=IDG(38)
      END
