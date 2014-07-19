*
* $Id: kaxe2i.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxe2i.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXE2I(STRING,INT,*)
*
*.....CONVERT A CHARACTER STRING FROM EXTERNAL TO INTERNAL FORMAT
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax040.inc"
*
      CHARACTER STRING*(*)
*
      INTEGER INT(*)
*
      LOGICAL NUMBER
*
*-----------------------------------------------------------------------
*
*.....CALCULATE THE NUMBER OF WORDS IN THE INTERNAL REPRESENTATION
      NCHARS = LEN(STRING)
      INT(1) = (NCHARS-1)/MAXPAK + 2
*
*.....ASSUME THE STRING IS ALPHANUMERIC
      NUMBER = .FALSE.
      IE     = 0
      IW     = 1
*
*.....DETERMINE WHETHER THE STRING IS NUMERIC OR ALPHANUMERIC
      DO  1  I = 1, NCHARS
         IF ( INDEX('0123456789',STRING(I:I)) .LE. 0 ) GO TO 2
 1    CONTINUE
*
*.....THE STRING IS NUMERIC
      NUMBER = .TRUE.
      IE     = MOD(NCHARS,MAXPAK)
      IF ( IE .NE. 0 )  IW = IW + 1
      INT(2) = 0
*
*.....CONVERT THE STRING
 2    DO  3  I = 1, NCHARS
         IF ( IE .EQ. 0 )  THEN
            IE      = MAXPAK
            IW      = IW + 1
            INT(IW) = 0
         ENDIF
         IE = IE - 1
         DO  4  IC = 1, MAXC
            IF ( STRING(I:I) .EQ. CSET(IC) )  THEN
               INT(IW) = INT(IW)  +  IC * 100**IE
               GO TO 3
            ENDIF
 4       CONTINUE
         RETURN 1
 3    CONTINUE
*
      IF ( NUMBER )  INT(2) = -INT(2)
*
      RETURN
*
      END
