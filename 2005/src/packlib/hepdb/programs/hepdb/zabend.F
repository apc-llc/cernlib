*
* $Id: zabend.F,v 1.2 1996/03/12 13:09:17 cernlib Exp $
*
* $Log: zabend.F,v $
* Revision 1.2  1996/03/12 13:09:17  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:29  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE ZABEND
#include "zebra/zstate.inc"
      PRINT *,'ZABEND routine for HEPDB reached - ZPOSTM cancelled'
      IF(NQPHAS.LE.0) CALL ABEND
      NQPHAS = - 2
      CALL ZEND
      END
