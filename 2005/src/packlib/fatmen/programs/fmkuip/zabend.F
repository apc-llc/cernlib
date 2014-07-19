*
* $Id: zabend.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: zabend.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE ZABEND
#include "zebra/zstate.inc"
      PRINT *,'ZABEND routine for FATMEN reached - ZPOSTM cancelled'
      IF(NQPHAS.LE.0) CALL ABEND
      NQPHAS = - 2
      CALL ZEND
      END
 
