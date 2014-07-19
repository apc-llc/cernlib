*
* $Id: igerr.F,v 1.1.1.1 1996/03/01 11:38:37 mclareni Exp $
*
* $Log: igerr.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:37  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_IBM)
*CMZ :  2.07/00 11/04/95  13.53.22  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE IGERR(CHMESS,CHROUT)
      CHARACTER*(*) CHROUT,CHMESS
      IF(CHROUT.EQ.' ')THEN
         CALL HBUG(CHMESS,'?? ',0)
      ELSE
         CALL HBUG(CHMESS,CHROUT,0)
      ENDIF
      END
#endif
