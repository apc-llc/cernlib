*
* $Id: hlswap.F,v 1.1.1.1 1996/01/16 17:07:42 mclareni Exp $
*
* $Log: hlswap.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 18/11/92  18.21.19  by  Rene Brun
*-- Author :
      SUBROUTINE HLSWAP(A,B,I,J)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PART OF HSORT
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER A(*),B(*),I,J,K
      DO 10 K=1,4
         A(4*(I-1)+K)=B(4*(J-1)+K)
 10   CONTINUE
      END
C
