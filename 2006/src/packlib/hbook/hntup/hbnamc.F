*
* $Id: hbnamc.F,v 1.1.1.1 1996/01/16 17:07:56 mclareni Exp $
*
* $Log: hbnamc.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:56  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 18/12/92  15.29.11  by  Unknown
*-- Author :    Fons Rademakers   17/12/91
      SUBROUTINE HBNAMC(ID, BLKNAM, ADDRES, FORM)
*.==========>
*.
*.           Describing the variables to be stored in the new
*.           (variable row length) n-tuple.
*.           ADDRES must be the address of a character variable.
*.           For the data-structure description see routine HBNT.
*.
*..=========> ( A.A.Rademakers )
*
      INTEGER       ID
      CHARACTER*(*) BLKNAM, ADDRES(1), FORM
*
#if !defined(CERNLIB_VAX)
      IADD = LOCB(ADDRES(1))
#endif
#if defined(CERNLIB_VAX)
      IADD = %LOC(ADDRES(1))
#endif
*
      CALL HBNAM(ID, BLKNAM, IADD, FORM, .TRUE.)
*
      END
