*
* $Id: gustep.F,v 1.1.1.1 1996/03/06 15:36:26 mclareni Exp $
*
* $Log: gustep.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:26  mclareni
* Add geane321 examples
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      SUBROUTINE GUSTEP
*
#include "geant321/gcflag.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcvolu.inc"
*
      COMMON /COORD/CTRU(6,10),NMEA(10),charg
*
      INTEGER IWBOX
      LOGICAL FIRST
*
      SAVE IWBOX,FIRST
*
      DATA FIRST/.TRUE./
*______________________________________________________
*
*
*   * Initialization
*
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL UCTOH ('WBOX', IWBOX, 4, 4)
      ENDIF
*
**     DEBUG
*
      IF (IDEBUG*ISWIT(2).NE.0) CALL GPCXYZ
*      IF (IDEBUG*ISWIT(2).NE.0) CALL GSXYZ
      CALL GSXYZ
*
**    store measured coordinate
*
      IF (NAMES(NLEVEL).EQ.IWBOX) THEN
        NMEA(NUMBER(3)) = NMEA(NUMBER(3)) + 1
        CTRU(1,NUMBER(3)) = CTRU(1,NUMBER(3)) + VECT(1)
        CTRU(2,NUMBER(3)) = CTRU(2,NUMBER(3)) + VECT(2)
        CTRU(3,NUMBER(3)) = CTRU(3,NUMBER(3)) + VECT(3)
        CTRU(4,NUMBER(3)) = CTRU(4,NUMBER(3)) + VECT(4)*VECT(7)
        CTRU(5,NUMBER(3)) = CTRU(5,NUMBER(3)) + VECT(5)*VECT(7)
        CTRU(6,NUMBER(3)) = CTRU(6,NUMBER(3)) + VECT(6)*VECT(7)
      ENDIF
*
      END
