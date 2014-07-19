*
* $Id: mpsav.F,v 1.1.1.1 1996/02/26 17:16:42 mclareni Exp $
*
* $Log: mpsav.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:42  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.05  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE MPSAV(IND)
      COMMON /MDPOOL/IQ(6)
      NUSP=IQ(IND+6)
      IF(NUSP.EQ.0) RETURN
      J=IQ(IND+7)
      DO 1 K=1,NUSP
      IF(IQ(J).GT.0) GO TO 1
#if defined(CERNLIB_ASSEMB)
      CALL CSBCLL(IQ(IND+15),1,IQ(IND),IQ(J+1),IQ(J+4),IQ(IND+3))
#endif
#if !defined(CERNLIB_ASSEMB)
      JSA=IQ(IND+15)
      CALL CSBCLL(%VAL(JSA),5,1,IQ(IND),IQ(J+1),IQ(J+4),IQ(IND+3))
#endif
      IQ(IND+11)=IQ(IND+11)+1
      IQ(J)=-IQ(J)
    1 J=IQ(J+2)
      RETURN
      END
