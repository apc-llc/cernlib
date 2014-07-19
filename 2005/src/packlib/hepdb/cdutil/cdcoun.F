*
* $Id: cdcoun.F,v 1.1.1.1 1996/02/28 16:24:37 mclareni Exp $
*
* $Log: cdcoun.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:37  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCOUN(ICOUNT)
*
*     Count the that files are currently attached
*
#include "zebra/rzcl.inc"
#include "zebra/zunit.inc"
 
      ICOUNT = 0
      IF(LQRS.EQ.0) GOTO 30
      LRZ = LQRS
  10  IF(LRZ.EQ.0)  GOTO 30
      LUNDB = IQ(KQSP+LRZ-5)
      IF(LUNDB.EQ.0) GOTO 20
      ICOUNT = ICOUNT + 1
  20  CONTINUE
      LRZ=LQ(KQSP+LRZ)
      GO TO 10
  30  CONTINUE
 
      END
