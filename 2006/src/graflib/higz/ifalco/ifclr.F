*
* $Id: ifclr.F,v 1.2 1996/03/07 17:38:08 couet Exp $
*
* $Log: ifclr.F,v $
* Revision 1.2  1996/03/07 17:38:08  couet
* IFALCO selection was wrong
*
* Revision 1.1.1.1  1996/02/14 13:10:30  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 11/12/91  18.16.59  by  O.Couet
*-- Author :    O.Couet   11/12/91
      SUBROUTINE IFCLR(IWTYP)
      IF(IWTYP.EQ.7878)THEN
         CALL IFPUT(29)
         CALL IFPUT(27)
         CALL IFPUT(12)
         CALL IFPUT(0)
         CALL IFPUT(31)
         CALL IFPUT(24)
         CALL IFPUT(0)
      ELSEIF(IWTYP.EQ.7879)THEN
         CALL IGSG(0)
         CALL IFPUT(27)
         CALL IFPUT(12)
         CALL IFPUT(0)
         CALL IGSA(0)
      ENDIF
      END
