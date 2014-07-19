*
* $Id: pfindf.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: pfindf.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/20 30/09/94  19.54.29  by  Fons Rademakers
*-- Author :    Fons Rademakers   24/01/94
      SUBROUTINE PFINDF(CADR1, IERROR)
*.==========>
*.
*.     Loop over the routine referenced via CADR and all routines in
*.     its calling tree.
*.
*..=========> ( A.A.Rademakers )
*
      PARAMETER (MAXCAL = 25)
      INTEGER   CADR, CADR1, JADR(0:MAXCAL), NEXT(MAXCAL), NLEV
      INTEGER   CSLISTC
*
      IERROR = 0
*
      CALL PFINDF1(CADR1, IERROR)
      IF (IERROR .NE. 0) GOTO 90
*
      NLEV = 0
      JADR(NLEV) = CADR1
      CADR = JADR(NLEV)
*
  10  NLEV = NLEV + 1
      IF (NLEV .GT. MAXCAL) THEN
         PRINT *, 'PFINDF: Calling tree deeper than ', MAXCAL
         IERROR = 1
         GOTO 90
      ENDIF
      NEXT(NLEV) = 0
  20  JADR(NLEV) = CSLISTC(CADR, NEXT(NLEV))
      IF (JADR(NLEV) .NE. 0) THEN
         CADR = JADR(NLEV)
         CALL PFINDF1(CADR, IERROR)
         IF (IERROR .NE. 0) GOTO 90
         GOTO 10
      ELSE
         NLEV = NLEV - 1
         IF (NLEV .EQ. 0) GOTO 90
         CADR = JADR(NLEV-1)
         GOTO 20
      ENDIF
*
  90  CONTINUE
*
      END
