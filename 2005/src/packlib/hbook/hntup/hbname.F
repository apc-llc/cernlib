*
* $Id: hbname.F,v 1.1.1.1 1996/01/16 17:07:56 mclareni Exp $
*
* $Log: hbname.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:56  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/00 26/05/92  19.39.53  by  Fons Rademakers
*-- Author :    Fons Rademakers   17/12/91
      SUBROUTINE HBNAME(ID, BLKNAM, ADDRES, FORM)
*.==========>
*.
*.           Describing the variables to be stored in the new
*.           (variable row length) n-tuple.
*.           ADDRES must be the address of a integer or real variable.
*.           For the data-structure description see routine HBNT.
*.
*..=========> ( A.A.Rademakers )
*
      INTEGER       ID, ADDRES(1)
      CHARACTER*(*) BLKNAM, FORM
*
      IADD = LOCB(ADDRES(1))
*
      CALL HBNAM(ID, BLKNAM, IADD, FORM, .FALSE.)
*
      END
