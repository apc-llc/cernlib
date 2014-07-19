*
* $Id: mkdigi.F,v 1.1.1.1 1996/02/26 17:16:58 mclareni Exp $
*
* $Log: mkdigi.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:58  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)||!defined(CERNLIB_ASSEMB)
*CMZ :  1.18/00 18/02/94  12.25.04  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION MKDIGI(N)
      IF(N.LT.0 .OR. N.GT.9) GO TO 1
      MKDIGI=ICHAR('0')+N
      RETURN
    1 MKDIGI=ICHAR('*')
      END
#endif
