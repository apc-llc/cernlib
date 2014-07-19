*
* $Id: hfpbug.F,v 1.4 1997/10/23 13:46:33 mclareni Exp $
*
* $Log: hfpbug.F,v $
* Revision 1.4  1997/10/23 13:46:33  mclareni
* NT mods
*
* Revision 1.3  1997/02/27 16:19:28  couet
* - Wrong coding of the error messages
*
* Revision 1.2  1997/02/27 14:44:59  couet
* - wrong comparison. X was used instead of I
*
* Revision 1.1.1.1  1996/01/16 17:07:38  mclareni
* First import
*
#include "hbook/pilot.h"
*-- Author :    Francois Dardare   24/04/95
*----------------------------------------------------------------------------*
*									     *
* 	function called when the floating point is not finit                 *
*       it gives an accurate classification of it			     *
*									     *
*----------------------------------------------------------------------------*
      SUBROUTINE HFPBUG(X,ROUT,ID)
      CHARACTER*(*) ROUT
#ifndef CERNLIB_WINNT
      I=IFPSCL(X)
#else
      I = 0;
      CALL HBUG('WIN32 can not recognise this bug',ROUT,ID)
#endif
      IF (I .EQ. 5) THEN
        CALL HBUG('Negative infinity',ROUT,ID)
      ELSEIF (I .EQ. 4) THEN
        CALL HBUG('Positive infinity',ROUT,ID)
      ELSEIF (I .EQ. 8) THEN
        CALL HBUG('Signaling NaN',ROUT,ID)
      ELSE
        CALL HBUG('Quiet NaN',ROUT,ID)
      ENDIF
      END
