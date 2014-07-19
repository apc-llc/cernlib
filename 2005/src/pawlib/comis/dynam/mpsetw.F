*
* $Id: mpsetw.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: mpsetw.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE MPSETW(IND,IADR,IW)
      COMMON /MDPOOL/ IQ(6)
    1 I=MPIREC(IND,-IADR-1,NRES)
      IQ(I)=IW
      RETURN
      END
