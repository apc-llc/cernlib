*
* $Id: dzedrz.F,v 1.1.1.1 1996/03/04 16:13:21 mclareni Exp $
*
* $Log: dzedrz.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:21  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEDRZ
 
*     list directory of RZ-file
#include "dzeditkeep.inc"
 
      IF(LUNRZ.EQ.0)THEN
         WRITE(LUNOUT,*)' No RZ-file open'
      ELSE
         CALL RZLDIR(' ',' ')
      ENDIF
      CONTINUE
      END
 
***********************************************************************
 
