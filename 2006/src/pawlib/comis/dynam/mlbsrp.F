*
* $Id: mlbsrp.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: mlbsrp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLBSRP(ID,KEY)
      COMMON /MDPOOL/ IQ(6)
      MLBSRP=0
      I=MLBXSP(ID,IA,IR,KEY)
      IF(I.EQ.0) RETURN
      LOFF=IQ(ID+11)
      IF(IQ(ID+2).EQ.0) LOFF=IQ(I+2)
      MLBSRP=I+LOFF
      RETURN
      END
