*
* $Id: iatr3.F,v 1.3 1998/01/28 11:21:46 couet Exp $
*
* $Log: iatr3.F,v $
* Revision 1.3  1998/01/28 11:21:46  couet
* - is in fact used. but should not be. Need a closer look to remove it
*
* Revision 1.1.1.1  1996/02/14 13:10:42  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.14/00 06/02/92  10.46.07  by  O.Couet
*-- Author :    Gareth Richards   15/07/91
      SUBROUTINE IATR3(X,Y,Z,XX,YY,ZZ,TEXT)
#include "higz/hiatt.inc"
      CHARACTER*(*) TEXT
      IF (IDIM.EQ.3) THEN
#if defined(CERNLIB_PHIGS)
         CALL PATR3(X,Y,Z,XX,YY,ZZ,TEXT)
#endif
      ENDIF
*
      END
