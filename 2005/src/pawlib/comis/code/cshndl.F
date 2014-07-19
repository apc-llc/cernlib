*
* $Id: cshndl.F,v 1.1.1.1 1996/02/26 17:16:23 mclareni Exp $
*
* $Log: cshndl.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:23  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.14/00 08/02/93  09.28.28  by  Alfred Nathaniel
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSHNDL(ISIG,MECH)
***----------------------------------------
*     it is condition handler: for vax only
***----------------------------------------
      INTEGER ISIG(*),MECH(*)
      MECH(4)=-777
#if defined(CERNLIB_VAX)
      CALL SYS$PUTMSG(ISIG,,)
      CALL LIB$SIG_TO_RET(ISIG,MECH)
#endif
      CSHNDL=1
      END
