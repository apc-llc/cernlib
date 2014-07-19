*
* $Id: cdclsh.F,v 1.2 1996/03/12 13:08:30 cernlib Exp $
*
* $Log: cdclsh.F,v $
* Revision 1.2  1996/03/12 13:08:30  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCLSH
*
*     Close a HEPDB database file
*
#include "hepdb/cdunit.inc"
#include "zebra/rzcl.inc"
#include "zebra/zunit.inc"
 
      CHARACTER*16  CHDIR,CHTOP,CHNAME
      CHARACTER*8   CHOPT
#include "hepdb/hdboptd.inc"
      DIMENSION     IHDIR(4)
 
      CALL KUGETC(CHDIR,LDIR)
 
      IF(CHDIR(1:2).EQ.'//') THEN
         CHTOP = CHDIR(3:LDIR)
         LTOP  = LDIR - 2
      ELSEIF(LDIR.EQ.2) THEN
         CHTOP = 'CD'//CHDIR(1:LDIR)
         LTOP  = LDIR + 2
      ELSE
         CHTOP = CHDIR(1:LDIR)
         LTOP  = LDIR
      ENDIF
 
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
         IF(LLOGCD.GE.0) WRITE(LPRTCD,*) 'CDCLSH. closing ',
     +      CHNAME(1:LN),' on unit ',LUNDB
*
*     Now issue CDEND for this file
*
         CALL CDEND(CHNAME(1:LN),' ',IRC)
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
         IF(LLOGCD.GE.-3) WRITE(LPRTCD,*) 'CDCLSH. file with ',
     +      'top directory ',CHTOP(1:LTOP),' is not open'
      ENDIF
 
      IF(LLOGCD.GE.0) WRITE(LPRTCD,*) 'CDCLSH. closed ',
     +   NCLOSD,' file(s)'
 
      END
