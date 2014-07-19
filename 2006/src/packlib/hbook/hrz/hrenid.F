*
* $Id: hrenid.F,v 1.2 1997/01/17 09:10:41 couet Exp $
*
* $Log: hrenid.F,v $
* Revision 1.2  1997/01/17 09:10:41  couet
* - This routine didn't work for more that one histogram because some
* initialisation was missing. Note that the problem was not vissible on
* HPUX 9 but was on all other platforms.
*
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          24/10/95  14.32.18  by  Julian Bunn
*-- Author :    Rene Brun   13/11/91
      SUBROUTINE HRENID(IDOLD,IDNEW)
*.==========>
*.           To Rename Object IDOLD to IDNEW in the current
*.           directory on RZ file
*..=========> ( R.Brun )
#include "hbook/hcdire.inc"
      COMMON/QUEST/IQUEST(100)
      DIMENSION KIDOLD(2),KIDNEW(2)
*.___________________________________________
*
*         Check if IDOLD exists
*
      KIDOLD(1)=IDOLD
      KIDOLD(2)=0
      CALL RZINK(KIDOLD,99999,' ')
      IF(IQUEST(1).NE.0)THEN
         CALL HBUG('Unknown histogram','HRENID',IDOLD)
         GO TO 99
      ENDIF
*
*        Check if IDNEW already exists
*
      KIDNEW(1)=IDNEW
      KIDNEW(2)=0
      CALL RZINK(KIDNEW,99999,' ')
      IF(IQUEST(1).EQ.0)THEN
         CALL HBUG('Already existing identifier: cannot rename',
     +              'HRENID',IDNEW)
         IQUEST(1)=0
         GO TO 99
      ENDIF
*
*        Rename top object
*        Check if ntuple extensions exist, if YES must loop
*
      CALL RZCDIR(' ',' ')
      NKEYS = IQUEST(7)
      DO 10 IKEY=1,NKEYS
         CALL RZINK(IKEY,99999,'SC')
         IF(IQUEST(1).NE.0)GO TO 90
         IF(IQUEST(21).NE.IDOLD)GO TO 10
         KIDOLD(1)=IDOLD
         KIDOLD(2)=IQUEST(22)
         KIDNEW(1)=IDNEW
         KIDNEW(2)=IQUEST(22)
         CALL RZRENK(KIDOLD,KIDNEW)
         IF(IQUEST(1).NE.0)GO TO 90
  10  CONTINUE
*
  90  CALL RZSAVE
  99  END
