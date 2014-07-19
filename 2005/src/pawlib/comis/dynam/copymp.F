*
* $Id: copymp.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: copymp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE COPYMP(IND,IBUF,IADR,N)
      COMMON /MDPOOL/ IQ(6)
      COMMON /MPNORD/ NOREAD
      DIMENSION IBUF(N)
      I=1
      IK=-IADR-N
      IN=-IADR-1
    1 J=MPIREC(IND,IN,NRES)
      NOREAD=0
      IN=IN-NRES
      JL=IN-IK+1
      IF(JL.LT.0) NRES=NRES+JL
      CALL CCOPYA(IBUF(I),IQ(J),NRES)
      I=I+NRES
      IF(JL.GE.IQ(IND+3)) NOREAD=1
      IF(IN.GE.IK) GO TO 1
      RETURN
      END
