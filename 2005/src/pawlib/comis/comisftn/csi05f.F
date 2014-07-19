*
* $Id: csi05f.F,v 1.1.1.1 1996/02/26 17:16:51 mclareni Exp $
*
* $Log: csi05f.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:51  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_APOLLO)
*CMZ :  1.12/05 20/03/92  16.04.19  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSI05F(NAME,IAP)
      INTEGER IAP(10)
      INTEGER JARG1,JARG2,JARG3,JARG4,JARG5
      INTEGER IARG1,IARG2,IARG3,IARG4,IARG5
      POINTER/JARG1/IARG1
      POINTER/JARG2/IARG2
      POINTER/JARG3/IARG3
      POINTER/JARG4/IARG4
      POINTER/JARG5/IARG5
      JARG1=IAP(1)
      JARG2=IAP(2)
      JARG3=IAP(3)
      JARG4=IAP(4)
      JARG5=IAP(5)
      CSI05F=NAME(IARG1,IARG2,IARG3,IARG4,IARG5)
      END
#endif
