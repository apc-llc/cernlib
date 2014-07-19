*
* $Id: mdfree.F,v 1.1.1.1 1996/02/26 17:16:56 mclareni Exp $
*
* $Log: mdfree.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:56  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE MDFREE(IND,M)
      COMMON /MDPOOL/IQ(6)
      J=IND+2
      IF(IQ(IND)) 2,2,1
    1 J=IND-3
    2 J=IQ(J)
      CALL MDFH(IND,M)
      RETURN
      END
