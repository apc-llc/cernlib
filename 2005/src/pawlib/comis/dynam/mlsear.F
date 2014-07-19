*
* $Id: mlsear.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: mlsear.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLSEAR(I,KEY)
      COMMON /MDPOOL/ IQ(6)
      MLSEAR=-1
      J=IQ(I)
      IF(J.EQ.0) RETURN
      MLSEAR=MLBSRP(J,KEY)
      RETURN
      END
