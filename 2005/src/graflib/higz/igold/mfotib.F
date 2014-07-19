*
* $Id: mfotib.F,v 1.1.1.1 1996/02/14 13:10:48 mclareni Exp $
*
* $Log: mfotib.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:48  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.58.00  by  Unknown
*-- Author : V.Fine
      SUBROUTINE MFOTIB(IVAL)
#include "higz/golden.inc"
*
      CALL MPUTBF(char(ival),1)
      END
#endif
