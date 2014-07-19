*
* $Id: mlins.F,v 1.1.1.1 1996/02/26 17:16:44 mclareni Exp $
*
* $Log: mlins.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:44  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLINS(I,KEY,L)
      INTEGER CIFNCL
      COMMON /MDPOOL/ IQ(6)
      MLINS=-1
      J=IQ(I)
      IF(J.EQ.0) RETURN
#if defined(CERNLIB_ASSEMB)
      MLINS=CIFNCL(IQ(I+2),J,KEY,L)
#endif
#if !defined(CERNLIB_ASSEMB)
      JSA=IQ(I+2)
      MLINS=CIFNCL(%VAL(JSA),3,J,KEY,L)
#endif
      RETURN
      END
