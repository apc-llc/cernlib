*
* $Id: mxprnt.F,v 1.1.1.1 1996/04/01 15:03:17 mclareni Exp $
*
* $Log: mxprnt.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:17  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE MXPRNT(A,IDIM1,IDIM2,IROW,ICOL,NCOL,FORMAT)
      DIMENSION A(IDIM1,IDIM2),FRMT(7)
      DATA FRMT/8H(1H0,I5,,1H0,1H0,8H/(1X,I5,,1H0,1H0,2H))/
      FRMT(3)=FORMAT
      FRMT(6)=FRMT(3)
      DO 4 K=1,ICOL,NCOL
      L=K+NCOL-1
      IF(L-ICOL)3,3,2
    2 L=ICOL
    3 IT=L+1-K
      ENCODE(8,100,FRMT(2))IT
      FRMT(5)=FRMT(2)
    4 WRITE(6,FRMT(1))(I,(A(I,J),J=K,L),I=1,IROW)
      RETURN
  100 FORMAT(I8)
      END
