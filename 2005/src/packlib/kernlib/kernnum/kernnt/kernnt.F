*
* $Id: kernnt.F,v 1.1.1.1 1996/02/15 17:48:34 mclareni Exp $
*
* $Log: kernnt.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:34  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE KERNNT(LUN)
C
C
C         *********************************************
C         *                                           *
C         *         PRINT PAM TITLE ON LUN            *
C         *                                           *
C         *********************************************
C
          WRITE(LUN,1000)
          RETURN
 1000     FORMAT('0PACKAGE COMPILED FROM PAM FILE= ',
     + 'KERNNUM  3.11/01   950529 18.00  '
     +/)
          END
