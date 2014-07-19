*
* $Id: mimpjw.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: mimpjw.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MIMPJW(IND,IADR)
      COMMON /MDPOOL/ IQ(6)
    1 I=MPIREC(IND,IADR,NRES)
      MIMPJW=IQ(I)
      RETURN
      END
