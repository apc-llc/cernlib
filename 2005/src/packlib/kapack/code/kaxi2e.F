*
* $Id: kaxi2e.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaxi2e.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXI2E(INT,STRING,*)
*
*.....CONVERT A CHARACTER STRING FROM INTERNAL TO EXTERNAL FORMAT
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax040.inc"
#include "kapack/kax050.inc"
*
      CHARACTER STRING*(*)
*
      INTEGER INT(*)
*
*-----------------------------------------------------------------------
*
*.....CALCULATE THE NUMBER OF NON-NULL CHARACTERS IN THE STRING
      NCHARS = (INT(1)-1)*MAXPAK
      NUMBER = INT(INT(1))
      IF ( INT(2) .LT. 0 ) NUMBER = ABS(INT(2))
*
      DO  1  I = 1, MAXPAK
         IF ( MOD(NUMBER,100) .EQ. 0 )  NCHARS = NCHARS - 1
         NUMBER = NUMBER/100
 1    CONTINUE
*
*.....CHECK THAT THE NUMBER OF CHARACTERS IS NOT TOO LARGE
      IF ( NCHARS .GT. LEN(STRING) ) RETURN 1
*
*.....DO THE CONVERSION
      STRING = ' '
      DO  3  IW = INT(1), 2, -1
         NUMBER = ABS(INT(IW))
         DO  2  I = 1, MAXPAK
            IC = MOD(NUMBER,100)
            IF ( IC .NE. 0 )  THEN
               IF ( IC.LT.1 .OR. IC.GT.MAXC )  GO TO 91
               STRING(NCHARS:NCHARS) = CSET(IC)
               NCHARS = NCHARS - 1
            ENDIF
            NUMBER = NUMBER/100
 2       CONTINUE
 3    CONTINUE
*
      RETURN
*
*.....ERROR PROCESSING
 91   WRITE(MSG,191) IC
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 191  FORMAT('CRNKA151 KAXI2E: KAPACK SYSTEM ERROR OR CORRUPT FILE ON
     &        UNIT ',I3,', INVALID CODE ',I11,' IN NAME')
*
      END
