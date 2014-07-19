*
* $Id: pawintm.F,v 1.1.1.1 1996/03/01 11:38:43 mclareni Exp $
*
* $Log: pawintm.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/05 25/11/93  16.08.50  by  Rene Brun
*-- Author :    Fons Rademakers   20/11/93
      SUBROUTINE PAWINTM(CLASS)
*
*  Motif initialization of PAW++ (IWK = 999).
*  Using CLASS one can specify the X resource class.
*  This routine must be called after PAWINT2 and before PAWINT3.
*
#include "paw/pcwk.inc"
*
      CHARACTER*(*)  CLASS
*
      IF (IWK .EQ. 999) THEN
         CALL PMINIT
         CALL KUINIM(CLASS(1:LENOCC(CLASS)))
*
      ENDIF
*
      END
