*
* $Id: epdrop.F,v 1.1.1.1 1996/03/08 15:21:43 mclareni Exp $
*
* $Log: epdrop.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:43  mclareni
* Epio
*
*
#include "epio/pilot.h"
      SUBROUTINE EPDROP(LUNIT,IERR)
C. DROPS AN ACTIVE UNIT, SETS THE SPACE FREE.
C. INPUT
C. LUNIT     LOGICAL UNIT NUMBER
C. OUTPUT
C. IERR      ERROR CONDITION
#include "epio/epiocom.inc"
#include "epio/epvaxcom.inc"
* Ignoring t=pass
#include "epio/epapocom.inc"
* Ignoring t=pass
      IERR=0
      IF(LUNIT.NE.LASTUT)  CALL EPUREF(LUNIT)
      IF(LREF.EQ.0)  GOTO 9901
C--- DROP INTERNAL UNIT LREF
      IF(LREF.EQ.LIST(1))  GOTO 3
      IL=NWUNIT*(LIST(1)-1)+NCONT+1
      CALL UCOPY(LIST(IL),LIST(NCONT+NWUNIT*(LREF-1)+1),NWUNIT)
    3 LIST(1)=LIST(1)-1
*
*---- BLANK DEFAULT FILE NAMES FOR VAX, APOLLO and UNIX
*
      DO 10  I=1,NMUNIT
  10  EPVXUN(I)=' '
C--- SET LAST USER UNIT FLAG TO ZERO TO FORCE EPUNIT CALL NEXT TIME
      LASTUT=0
      LREF=0
77777 RETURN
 9901 CONTINUE
C--- UNIT DOES NOT EXIST
      IERR=14
      CALL EPERRH(LUNIT,IERR)
      GOTO 77777
      END
