*
* $Id: cshext.F,v 1.1.1.1 1996/02/26 17:16:31 mclareni Exp $
*
* $Log: cshext.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:31  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_PAW)
*CMZ :  1.18/00 08/02/94  17.50.26  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   08/02/94
      SUBROUTINE CSHEXT(NAME,EXT,NOARGS)
      CHARACTER *(*)NAME
#include "comis/cshfill.inc"
      IF(MODHFI.GT.KNMSHFI)THEN
        PRINT *,'***CSHEXT: more then ',KNMSHFI,' hfills names'
      ELSE
        MODHFI=MODHFI+1
        HFINMS(MODHFI)=NAME
        JADHFI(MODHFI)=JUMPAD(EXT)
        NARGHFI(MODHFI)=NOARGS
      ENDIF
      END
#endif
