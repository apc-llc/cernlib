*
* $Id: cdrzop.F,v 1.2 1996/03/12 13:09:03 cernlib Exp $
*
* $Log: cdrzop.F,v $
* Revision 1.2  1996/03/12 13:09:03  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:31  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDRZOP
#include "hepdb/cdunit.inc"
#include "hepdb/quest.inc"
*
*     Open a HEPDB file
*
      CHARACTER*16  CHTOP
      CHARACTER*255 CHFILE
      CHARACTER*26  CHOPT
 
      CALL KUGETC(CHTOP,LTOP)
      CALL KUGETS(CHFILE,LFILE)
      CALL KUGETC(CHOPT,LOPT)
*
*     Get a FORTRAN unit
*
      CALL CDGLUN(LUNDB,IRC)
 
      IF(IRC.NE.0) THEN
         IF(LLOGCD.GT.-3) WRITE(LPRTCD,*) 'CDRZOP. could not ',
     +      'allocate FORTRAN unit to open file'
         RETURN
      ENDIF
 
      IF(LLOGCD.GE.3) WRITE(LPRTCD,*) 'CDRZOP. allocated FORTRAN unit ',
     +   LUNDB
*
*     Build top directory name, if required
*
      IF(LTOP.EQ.0) THEN
         IF(LUNDB.LT.10) THEN
            WRITE(CHTOP,9001) LUNDB
            LTOP = 4
         ELSE
            WRITE(CHTOP,9002) LUNDB
            LTOP = 5
         ENDIF
 9001    FORMAT('LUN',I1)
 9002    FORMAT('LUN',I2)
      ENDIF
*
*     Open the file
*
      IF(LLOGCD.GE.3) WRITE(LPRTCD,*) 'CDRZOP. call RZOPEN for ',
     +   'LUN = ',LUNDB,' CHTOP = ',CHTOP,' CHFILE = ',
     +   CHFILE(1:LFILE),' options = ',CHOPT(1:LOPT)
 
      LRECL = 0
      CALL RZOPEN(LUNDB,CHTOP(1:LTOP),CHFILE(1:LFILE),
     +            CHOPT(1:LOPT),LRECL,IRC)
 
      IF(IRC.NE.0.AND.LLOGCD.GT.-3) THEN
         WRITE(LPRTCD,*) 'CDRZOP. cannot open ',CHFILE(1:LFILE),
     +                   ' return code from RZOPEN = ',IRC
         CALL CDFLUN(LUNDB,IRC)
      ELSEIF(IRC.EQ.0.AND.LLOGCD.GE.0) THEN
         WRITE(LPRTCD,*) 'CDRZOP. opened file ',CHFILE(1:LFILE),
     +                   ', top directory = ',CHTOP(1:LTOP)
*
*     Issue RZFILE
*
         CALL RZFILE(LUNDB,CHTOP(1:LTOP),CHOPT(1:LOPT))
         IF(IQUEST(1).NE.0.AND.LLOGCD.GT.-3) THEN
            WRITE(LPRTCD,*) 'CDRZOP. return code from RZFILE = ',
     +                      IQUEST(1)
         ENDIF
      ENDIF
 
      END
