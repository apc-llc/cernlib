*
* $Id: cdrdda.F,v 1.1.1.1 1996/02/28 16:24:51 mclareni Exp $
*
* $Log: cdrdda.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:51  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDRDDA (DATA)
*     ========================
*
************************************************************************
*                                                                      *
*        SUBR. CDRDDA (DATA*)                                          *
*                                                                      *
*   Routine to Decode the Data from the Display file                   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     DATA     Data Items                                              *
*                                                                      *
*   Called by CDUPKY                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/cxlink.inc"
      CHARACTER       IOC*1, STRFIL*80, CHVAL*80
      DIMENSION       DATA (9)
*
*     ------------------------------------------------------------------
*
*  ** Decode the Data part of the edited file
*
      REWIND (LUDACX)
      READ (LUDACX, 1001, ERR=10, END=10)
*
      I = 1
    2 CONTINUE
      READ (LUDACX, 1002, ERR=10, END=10) STRFIL
      CHVAL = ' '
      NCH = LENOCC (STRFIL)
      NCH = MIN0 (NCH, 80)
      IF (NCH.EQ.0) GO TO 10
      DO 3 J = 1, NCH
        IF (STRFIL(J:J).NE.' ') THEN
          IOC = STRFIL(J:J)
          JST = J + 6
          GO TO 4
        ENDIF
    3 CONTINUE
      IOC   = ' '
      JST   = 7
    4 LCDAT = 4
      LWRDH = 1
      DO 5 J = JST, NCH
        IF (STRFIL(J:J).NE.' ') THEN
          CHVAL = STRFIL(J:NCH)
          LCDAT = NCH-J+1
          LWRDH = (LCDAT-1)/4 + 1
          GO TO 6
        ENDIF
    5 CONTINUE
    6 CONTINUE
      IF (IOC.EQ.'B') THEN
        CALL CDCTOB (CHVAL, DATA(I), IRC)
        I = I + 1
      ELSE IF (IOC.EQ.'I') THEN
        CALL CDCHTI (CHVAL, ISTR, IRC)
        CALL UCOPY (ISTR, DATA(I), 1)
        I = I + 1
      ELSE IF (IOC.EQ.'F') THEN
        CALL CDCTOR (CHVAL, DATA(I), IRC)
        I = I + 1
      ELSE IF (IOC.EQ.'H') THEN
        CALL UCTOH (CHVAL, DATA(I), 4, LCDAT)
        I = I + LWRDH
      ELSE
        DATA(I) = 0.
        I = I + 1
      ENDIF
      GO TO 2
   10 CONTINUE
*
      CALL CDCLFL (LUDACX)
*
 1001 FORMAT (////)
 1002 FORMAT (A80)
*                                                             END CDRDDA
      END
