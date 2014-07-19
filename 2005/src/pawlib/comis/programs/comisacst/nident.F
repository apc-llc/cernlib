*
* $Id: nident.F,v 1.1.1.1 1996/02/26 17:16:38 mclareni Exp $
*
* $Log: nident.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:38  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION NIDENT(TXT,ID)
      CHARACTER *(*) TXT,ID
      J=MJSCHA(TXT)
      JI=MJSCHA(ID)
      N=LEN(TXT)
      NI=LEN(ID)
      NT=N
      K=MKBLAN(J,N)
      NT=NT-N
      NIDENT=MIDENT(J,N,JI,NI)
      IF(NIDENT.GT.0)NIDENT=NIDENT+NT
      END
