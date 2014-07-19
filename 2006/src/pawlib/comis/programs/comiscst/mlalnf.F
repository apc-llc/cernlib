*
* $Id: mlalnf.F,v 1.1.1.1 1996/02/26 17:16:43 mclareni Exp $
*
* $Log: mlalnf.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:43  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE MLALNF(LOP,IPAG,IND,L)
      DIMENSION IP(5),IAD(5)
      DATA IP/5*0/,IAD/5*0/
      IF(LOP.NE.0) RETURN
      IND=0
      DO 1 I=1,5
      ID=I
      IF(IP(I).EQ.0) GO TO 2
      IF(IP(I).EQ.IPAG) GO TO 3
    1 CONTINUE
      RETURN
    2 IP(ID)=IPAG
    3 IND=IAD(ID)
      IAD(ID)=IAD(ID)+L
      RETURN
      END
