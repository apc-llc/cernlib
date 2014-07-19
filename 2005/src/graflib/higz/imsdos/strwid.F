*
* $Id: strwid.F,v 1.1.1.1 1996/02/14 13:11:05 mclareni Exp $
*
* $Log: strwid.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:05  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.49.21  by  Unknown
*-- Author : V.Fine
      FUNCTION STRWID(CH,LCH)
#include "higz/hiatt.inc"
#include "higz/hikern.inc"
#include "higz/himeta.inc"
      CHARACTER*(*) CH
#include "higz/hiwcdc.inc"
      IF (ILOPWK(IWKNB) .EQ. IDMETA) THEN
         IF (IWTYL(IWKNB) .EQ. ISURF) THEN
            CALL IGTEXT(0.0,0.0,CH(:LCH),RCHH,A,'S')
            STRWID = IXWCDC(A)
         END IF
      ELSE
         STRWID = 8*MAG_X*lch-1
      END IF
      END
#endif
