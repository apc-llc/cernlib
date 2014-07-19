*
* $Id: ifmov9.F,v 1.2 1996/03/07 17:38:12 couet Exp $
*
* $Log: ifmov9.F,v $
* Revision 1.2  1996/03/07 17:38:12  couet
* IFALCO selection was wrong
*
* Revision 1.1.1.1  1996/02/14 13:10:31  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/08 18/01/94  14.33.00  by  O.Couet
*-- Author :
      SUBROUTINE IFMOV9(IX,IY)
#include "higz/hifc1.inc"
#include "higz/hifc6.inc"
      MFL=1
      IF (ISFL.NE.0) CALL IFPUT(29)
      CALL IFDRW9(IX,IY)
      END
