*
* $Id: miword.F,v 1.1.1.1 1996/02/26 17:16:50 mclareni Exp $
*
* $Log: miword.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:50  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MIWORD(JW)
#include "comis/cspar.inc"
#include "comis/comis.inc"
#include "comis/cspnts.inc"
      MIWORD=IA(JW-JTOPA)
      END
