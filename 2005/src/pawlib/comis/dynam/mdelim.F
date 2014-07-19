*
* $Id: mdelim.F,v 1.1.1.1 1996/02/26 17:16:58 mclareni Exp $
*
* $Log: mdelim.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:58  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MDELIM(K)
      MDELIM=(MLETTE(K)-1)*(MLDIGI(K)-1)
      RETURN
      END
