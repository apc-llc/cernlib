*
* $Id: mdexl.F,v 1.1.1.1 1996/02/26 17:16:56 mclareni Exp $
*
* $Log: mdexl.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:56  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      LOGICAL FUNCTION MDEXL(LENG,IND)
      COMMON /MDPOOL/ IQ(6)
      IF(MOD(LENG,2).EQ.1) LENG=LENG+1
      J=IQ(1)
      L=IQ(2)
      IF(J/2*2.NE.J) GO TO 1
      J=J+1
      L=L-1
    1 IF(LENG.LE.L) GO TO 2
      MDEXL=.FALSE.
      RETURN
    2 IND=J
      K=J+LENG
      IQ(1)=K
      IQ(2)=L-LENG
      IQ(K+1)=IQ(J+1)
      IQ(K)=IQ(J)
      MDEXL=.TRUE.
      RETURN
      END
