*
* $Id: zabend.F,v 1.1.1.1 1996/03/08 15:44:18 mclareni Exp $
*
* $Log: zabend.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:18  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZABEND
#include "cspack/zstate.inc"
      PRINT *,'ZABEND routine for ZFTP reached - ZPOSTM cancelled'
      IF(NQPHAS.LE.0) CALL ABEND
      NQPHAS = - 2
      CALL ZEND
      END
 
