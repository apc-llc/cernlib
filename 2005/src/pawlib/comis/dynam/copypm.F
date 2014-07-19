*
* $Id: copypm.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: copypm.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE COPYPM(IND,IADR,IBUF,N)
      COMMON /MDPOOL/ IQ(6)
      DIMENSION IBUF(N)
      I=1
      IN=IADR
      IK=IN+N
    1 J=MPIREC(IND,IN,NRES)
      IN=IN+NRES
      JL=IN-IK
      IF(JL.GT.0) NRES=NRES-JL
      CALL CCOPYA(IQ(J),IBUF(I),NRES)
      I=I+NRES
      IF(IN.LT.IK) GO TO 1
      RETURN
      END
