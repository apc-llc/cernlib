*
* $Id: color.F,v 1.1.1.1 1996/02/14 13:11:03 mclareni Exp $
*
* $Log: color.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:03  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.49.21  by  Unknown
*-- Author : V.Fine
      SUBROUTINE COLOR(INDEX)
#include "higz/hikern.inc"
#include "higz/hiatt.inc"
#include "higz/himeta.inc"
      I = INDEX
      IF (ILOPWK(IWKNB) .EQ. IDMETA) THEN
         IF (IWTYL(IWKNB) .EQ. ISURF) THEN
            CALL GOLWRI(SP,I,' ')
         END IF
      ELSE
         IER = SET_COLOR(I)
      END IF
      END
#endif
