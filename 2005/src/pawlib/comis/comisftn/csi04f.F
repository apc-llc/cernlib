*
* $Id: csi04f.F,v 1.1.1.1 1996/02/26 17:16:51 mclareni Exp $
*
* $Log: csi04f.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:51  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_APOLLO)
*CMZ :  1.12/05 20/03/92  16.04.19  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSI04F(NAME,IAP)
      INTEGER IAP(10)
      INTEGER JARG1,JARG2,JARG3,JARG4
      INTEGER IARG1,IARG2,IARG3,IARG4
      POINTER/JARG1/IARG1
      POINTER/JARG2/IARG2
      POINTER/JARG3/IARG3
      POINTER/JARG4/IARG4
      JARG1=IAP(1)
      JARG2=IAP(2)
      JARG3=IAP(3)
      JARG4=IAP(4)
      CSI04F=NAME(IARG1,IARG2,IARG3,IARG4)
      END
#endif
