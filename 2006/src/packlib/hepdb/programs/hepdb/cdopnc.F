*
* $Id: cdopnc.F,v 1.3 1996/04/01 08:16:17 jamie Exp $
*
* $Log: cdopnc.F,v $
* Revision 1.3  1996/04/01 08:16:17  jamie
* change idiv from 0 to 20
*
* Revision 1.2  1996/03/12 13:08:52  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:28  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDOPNC
#include "hepdb/cdunit.inc"
#include "hepdb/quest.inc"
*
*     Open a HEPDB file
*
      CHARACTER*16  CHTOP
      CHARACTER*2   CHPREF
      CHARACTER*255 CHFILE
      CHARACTER*26  CHOPT
 
      DATA          NENTRY/0/
      SAVE          NENTRY,LUNFZ
 
      IF(NENTRY.EQ.0) THEN
         CALL CDGLUN(LUNFZ,IRC)
         NENTRY = 1
         IDIV   = 20
      ENDIF
 
      CALL KUGETC(CHPREF,LPREF)
      CALL KUGETS(CHFILE,LFILE)
      CALL KUGETC(CHOPT,LOPT)
*
*     Get a FORTRAN unit
*
      CALL CDGLUN(LUNCD,IRC)
 
      IF(IRC.NE.0) THEN
         IF(LLOGCD.GT.-3) WRITE(LPRTCD,*) 'CDOPNC. could not ',
     +      'allocate FORTRAN unit to open file'
         RETURN
      ENDIF
 
      IF(LLOGCD.GE.3) WRITE(LPRTCD,*) 'CDOPNC. allocated FORTRAN unit ',
     +   LUNCD
*
*     Get database file name from prefix
*
      IF(LFILE.EQ.0) THEN
         CALL CDPREF(LUNCD,CHPREF,CHTOP,CHFILE,IRC)
         LFILE = LENOCC(CHFILE)
      ELSE
         CHTOP = 'CD'//CHPREF
      ENDIF
 
      LTOP  = LENOCC(CHTOP)
*
*     Open the database file
*
      LRECL  = 0
      CALL CDOPEN(LUNCD,LUNFZ,CHTOP,CHFILE,LRECL,IDIV,' ',IRC)
      IF(IRC.EQ.0) THEN
         IF(LLOGCD.GE.0) WRITE(LPRTCD,9001) CHFILE(1:LFILE),LUNCD,
     +      CHTOP(1:LTOP),LRECL
      ELSE
         IF(LLOGCD.GT.-3) WRITE(LPRTCD,9002) IRC,CHFILE(1:LFILE),LUNCD,
     +      CHTOP(1:LTOP),LRECL
      ENDIF
 
9001  FORMAT(' CDOPNC. opened file ',A,' on unit ',I2,
     +       ' with top directory ',A,' and record length ',I6)
9002  FORMAT(' CDOPNC. error ',I6,' opening  file ',A,' on unit ',I2,
     +       ' with top directory ',A,' and record length ',I6)
 
      END
