*
* $Id: mlput.F,v 1.1.1.1 1996/02/26 17:16:44 mclareni Exp $
*
* $Log: mlput.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:44  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLPUT(I,IR,INF,L)
      INTEGER CIFNCL
      COMMON /MDPOOL/ IQ(6)
      MLPUT=-1
      J=IQ(I)
      IF(J.EQ.0) RETURN
#if defined(CERNLIB_ASSEMB)
      MLPUT=CIFNCL(IQ(I+4),J,IR,INF,L)
#endif
#if !defined(CERNLIB_ASSEMB)
      JSA=IQ(I+4)
      MLPUT=CIFNCL(%VAL(JSA),4,J,IR,INF,L)
#endif
      RETURN
      END
