*
* $Id: kiargc.F,v 1.1.1.1 1996/03/08 15:32:55 mclareni Exp $
*
* $Log: kiargc.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:55  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.00/00 01/12/92  18.14.19  by  Alfred Nathaniel
*-- Author :    Alfred Nathaniel   01/12/92
      FUNCTION KIARGC()
*--- C interface to the intrinsic function IARGC
      KIARGC = IARGC()
      END
