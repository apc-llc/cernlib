*
* $Id: iuwk.F,v 1.1.1.1 1996/02/14 13:10:30 mclareni Exp $
*
* $Log: iuwk.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:30  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.12/16 13/08/91  16.29.43  by  O.Couet
*-- Author :
      SUBROUTINE IUWK(IWKID,IRFLG)
      IF (IRFLG.NE.0) CALL JUPDAT
      END
#endif
