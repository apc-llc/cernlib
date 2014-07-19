*
* $Id: abend.F,v 1.1.1.1 1996/02/15 17:48:36 mclareni Exp $
*
* $Log: abend.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:36  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE ABEND
#include "kernnumt/sysdat.inc"
          IF(LGFILE .EQ. 0)  WRITE(*,1000)
          IF(LGFILE .NE. 0)  WRITE(LGFILE,1000)
          RETURN
1000      FORMAT(31H ABEND ROUTINE HAS BEEN CALLED.)
          END
