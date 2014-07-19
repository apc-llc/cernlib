*
* $Id: cdfilc.F,v 1.2 1996/03/12 13:08:36 cernlib Exp $
*
* $Log: cdfilc.F,v $
* Revision 1.2  1996/03/12 13:08:36  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:28  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDFILC
*
*     Show what files are currently attached
*
#include "hepdb/cdunit.inc"
#include "zebra/rzcl.inc"
#include "zebra/zunit.inc"
 
      CHARACTER*16  CHNAME
      DIMENSION     IHDIR(4)
 
      NFILES = 0
      IF(LQRS.EQ.0) GOTO 30
      LRZ = LQRS
  10  IF(LRZ.EQ.0)  GOTO 30
      CALL ZITOH(IQ(KQSP+LRZ+1),IHDIR,4)
      CALL UHTOC(IHDIR,4,CHNAME,16)
      LN  = LENOCC(CHNAME)
      LUNDB = IQ(KQSP+LRZ-5)
      IF(LUNDB.EQ.0) GOTO 20
      NFILES = NFILES + 1
      WRITE(LOUTCD,9001) NFILES,LUNDB,CHNAME(1:LN)
9001  FORMAT(' File # ',I3.3,', unit: ',I4.4,', top directory: ',A16)
  20  CONTINUE
      LRZ=LQ(KQSP+LRZ)
      GO TO 10
  30  CONTINUE
 
      WRITE(LOUTCD,*) 'CDFILC. ',NFILES,' file(s) are attached'
 
      END
