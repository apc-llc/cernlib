*
* $Id: ifscur.F,v 1.2 1996/03/07 17:38:18 couet Exp $
*
* $Log: ifscur.F,v $
* Revision 1.2  1996/03/07 17:38:18  couet
* IFALCO selection was wrong
*
* Revision 1.1.1.1  1996/02/14 13:10:31  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/09 21/01/94  09.41.48  by  O.Couet
*-- Author :    John Clement   18/01/94
      SUBROUTINE IFSCUR(ICHR,XLOC,YLOC)
#include "higz/hifc2.inc"
      CHARACTER*16 STR
      ICHR=0
* Setup to find end of input
      STR=' '
* Put into GIN mode
      CALL IFPUT(27)
      CALL IFPUT(26)
      CALL IFPUT(-1)
      ISIZ=0
* Read the GIN string (Echoing is suppressed)
      READ(*,'(A)',END=99,ERR=99) STR
   10 CONTINUE
* IF <CR> pressed (Note it will be echoed now)
      IF (STR.EQ.' ') THEN
        READ(*,'(A)',END=99,ERR=99) STR(2:)
   20   CONTINUE
      ENDIF
* Find end of input
      ISIZ=INDEX(STR(2:),' ')
      IF (ISIZ.NE.5) RETURN
* Now get workstation Viewport coordinates.
      XLOC=128*(32*MOD(ICHAR(STR(2:2)),32)+MOD(ICHAR(STR(3:3)),32))
      YLOC=128*(32*MOD(ICHAR(STR(4:4)),32)+MOD(ICHAR(STR(5:5)),32))
* Now get Workstation window coordinates.
      XLOC=(XLOC-IXMIN)/FX+XMIN
      YLOC=(YLOC-IYMIN)/FY+YMIN
      ICHR=ICHAR(STR(1:1))
   99 CALL IFPUT(0)
      END
