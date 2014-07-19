*
* $Id: cdclsr.F,v 1.2 1996/03/12 13:08:31 cernlib Exp $
*
* $Log: cdclsr.F,v $
* Revision 1.2  1996/03/12 13:08:31  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCLSR
*
*     Close a HEPDB database file
*
#include "hepdb/cdunit.inc"
#include "zebra/rzcl.inc"
#include "zebra/zunit.inc"
 
      CHARACTER*16  CHTOP,CHNAME
      CHARACTER*8   CHOPT
#include "hepdb/hdboptd.inc"
      DIMENSION     IHDIR(4)
 
      CALL KUGETC(CHTOP,LTOP)
      CALL KUGETC(CHOPT,LOPT)
 
#include "hepdb/hdboptc.inc"
 
*
*     Find out the unit number for this file
*
      NCLOSD = 0
      IF(LQRS.EQ.0) GOTO 30
      LRZ = LQRS
  10  IF(LRZ.EQ.0)  GOTO 30
      CALL ZITOH(IQ(KQSP+LRZ+1),IHDIR,4)
      CALL UHTOC(IHDIR,4,CHNAME,16)
      LN  = LENOCC(CHNAME)
      IF(CHNAME(1:LN).EQ.CHTOP(1:LTOP).OR.IOPTA.NE.0) THEN
         LUNDB = IQ(KQSP+LRZ-5)
         IF(LUNDB.EQ.0) GOTO 20
         IF(LLOGCD.GE.0) WRITE(LPRTCD,*) 'CDCLSC. closing ',
     +      CHNAME(1:LN),' on unit ',LUNDB
         CALL RZCLOS(CHNAME(1:LN),' ')
         NCLOSD = NCLOSD + 1
*
*     Free the logical unit
*
         CALL CDFLUN(LUNDB,IRC)
      ENDIF
 
  20  CONTINUE
      LRZ=LQ(KQSP+LRZ)
      GO TO 10
  30  CONTINUE
 
      IF(IOPTA.EQ.0.AND.NCLOSD.EQ.0) THEN
         IF(LLOGCD.GE.-3) WRITE(LPRTCD,*) 'CDCLSC. file with ',
     +      'top directory ',CHTOP(1:LTOP),' is not open'
      ENDIF
 
      IF(LLOGCD.GE.0) WRITE(LPRTCD,*) 'CDCLSC. closed ',
     +   NCLOSD,' file(s)'
 
      END
