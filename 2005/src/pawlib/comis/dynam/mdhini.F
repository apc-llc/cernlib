*
* $Id: mdhini.F,v 1.1.1.1 1996/02/26 17:16:56 mclareni Exp $
*
* $Log: mdhini.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:56  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.01  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE MDHINI(IND,KEY)
      COMMON /MDPOOL/ IQ(6)
      IF(IQ(IND).LT.0) RETURN
      LENG=IQ(IND+3)
      LAV=IND+8
      IQ(IND)=-1
      IQ(IND+7)=0
      IQ(LAV)=0
      IQ(IND+11)=LAV
      N=IND+13
      M=14
      IF(KEY) 1,2,1
    1 IQ(IND)=-2
      N=N+2
      M=16
      IQ(IND+12)=0
      IQ(IND+13)=0
    2 IQ(IND+9)=N
      IQ(IND+10)=N
      LS=LENG-M
      K=IND+LENG-1
      IQ(K)=0
      IQ(K-1)=-LS
      IQ(IND+6)=K
      IQ(N-1)=0
      IQ(N)=-LS
      IQ(N+1)=LAV
      IQ(N+2)=LAV
      RETURN
      END
