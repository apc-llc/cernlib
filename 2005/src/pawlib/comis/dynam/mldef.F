*
* $Id: mldef.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: mldef.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLDEF(IND,MSEAR,MINS,MDEL,MPUT,MGET)
      COMMON /MDPOOL/ IQ(6)
      I=MDLOC(IND,6)
      IQ(I)=0
      IQ(I+1)=JUMPAD(MSEAR)
      IQ(I+2)=JUMPAD(MINS)
      IQ(I+3)=JUMPAD(MDEL)
      IQ(I+4)=JUMPAD(MPUT)
      IQ(I+5)=JUMPAD(MGET)
      MLDEF=I
      RETURN
      END
