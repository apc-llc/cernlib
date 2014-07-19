*
* $Id: csr03f.F,v 1.1.1.1 1996/02/26 17:16:52 mclareni Exp $
*
* $Log: csr03f.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:52  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_APOLLO)
*CMZ :  1.12/05 20/03/92  16.04.19  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION CSR03F(RNAME,IAP)
      INTEGER IAP(10)
      INTEGER JARG1,JARG2,JARG3
      INTEGER IARG1,IARG2,IARG3
      POINTER/JARG1/IARG1
      POINTER/JARG2/IARG2
      POINTER/JARG3/IARG3
      JARG1=IAP(1)
      JARG2=IAP(2)
      JARG3=IAP(3)
      CSR03F=RNAME(IARG1,IARG2,IARG3)
      END
#endif
