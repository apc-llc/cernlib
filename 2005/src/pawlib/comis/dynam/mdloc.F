*
* $Id: mdloc.F,v 1.1.1.1 1996/02/26 17:16:56 mclareni Exp $
*
* $Log: mdloc.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:56  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MDLOC(IND,L)
      COMMON /MDPOOL/IQ(6)
      I=IND+1
      IF(IQ(IND)) 2,2,1
    1 I=IND-4
    2 J=IQ(I)
      L1=L
      IF(MOD(L1,IQ(I+4)).EQ.1) L1=L1+1
      MDLOC=MDLH(IND,L1)
      RETURN
      END
