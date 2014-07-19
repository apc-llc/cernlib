*
* $Id: csi02f.F,v 1.1.1.1 1996/02/26 17:16:51 mclareni Exp $
*
* $Log: csi02f.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:51  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_APOLLO)
*CMZ :  1.12/05 20/03/92  16.04.19  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSI02F(NAME,IAP)
      INTEGER IAP(10)
      INTEGER JARG1,JARG2,JARG3
      INTEGER IARG1,IARG2,IARG3
      POINTER/JARG1/IARG1
      POINTER/JARG2/IARG2
      JARG1=IAP(1)
      JARG2=IAP(2)
      CSI02F=NAME(IARG1,IARG2)
      END
#endif
