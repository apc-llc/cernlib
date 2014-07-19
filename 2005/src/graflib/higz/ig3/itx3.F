*
* $Id: itx3.F,v 1.1.1.1 1996/02/14 13:10:43 mclareni Exp $
*
* $Log: itx3.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:43  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/06 14/11/95  10.26.40  by  O.Couet
*-- Author :    O.Couet   07/05/92
      SUBROUTINE ITX3(X,Y,Z,TEXT)
#include "higz/hi3d.inc"
      CHARACTER*(*) TEXT
      X2 = AWNX*X+BWNX*Y+CWNX*Z+DWNX
      Y2 = AWNY*X+BWNY*Y+CWNY*Z+DWNY
      Z2 = AWNZ*X+BWNZ*Y+CWNZ*Z+DWNZ
      CALL ITX(X2,Y2,TEXT)
*
      END
