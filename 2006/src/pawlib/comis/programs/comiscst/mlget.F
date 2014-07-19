*
* $Id: mlget.F,v 1.1.1.1 1996/02/26 17:16:41 mclareni Exp $
*
* $Log: mlget.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:41  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.05  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLGET(I,IR,INF,L)
      INTEGER CIFNCL
      COMMON /MDPOOL/ IQ(6)
      MLGET=-1
      J=IQ(I)
      IF(J.EQ.0) RETURN
#if defined(CERNLIB_ASSEMB)
      MLGET=CIFNCL(IQ(I+5),J,IR,INF,L)
#endif
#if !defined(CERNLIB_ASSEMB)
      JSR=IQ(I+5)
      MLGET=CIFNCL(%VAL(JSR),4,J,IR,INF,L)
#endif
      RETURN
      END
