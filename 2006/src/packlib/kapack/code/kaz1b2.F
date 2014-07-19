*
* $Id: kaz1b2.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaz1b2.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAZ1B2(INT)
*
*.....AUXILIARY ROUTINE TO KA1TO2: CONVERT A GIVEN NAME
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
*
      INTEGER INT(*)
*
      LOGICAL NEG
*
*-----------------------------------------------------------------------
*
      NEG    = INT(2) .LT. 0
      INT(2) = ABS( INT(2) )
*
*.....LOOP FOR EACH WORD IN THE NAME
      DO  2  IWORD = 2, INT(1)
         NUMOLD = INT(IWORD)
         NUMNEW = 0
         DO  1  I = 1, MAXPAK
            IC     = MOD(NUMOLD,100)
            NUMOLD = NUMOLD/100
            IF ( IC.GT.0 .AND. IC.LT.MAXC )  IC = IC + 1
            NUMNEW = NUMNEW  +  IC * 100**(I-1)
 1       CONTINUE
         INT(IWORD) = NUMNEW
 2    CONTINUE
*
      IF ( NEG )  INT(2) = -INT(2)
*
      END
