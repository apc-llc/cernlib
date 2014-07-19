*
* $Id: kaxcmp.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxcmp.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      INTEGER FUNCTION KAXCMP(KEYA,KEYB)
*
*.....COMPARE TWO KEYS IN INTERNAL REPRESENTATION
*           KEYA < KEYB  --->  KAXCMP < 0
*           KEYA = KEYB  --->  KAXCMP = 0
*           KEYA > KEYB  --->  KAXCMP > 0
*
      INTEGER KEYA(*), KEYB(*)
*
*-----------------------------------------------------------------------
*
      IF ( KEYA(2).LT.0 .AND. KEYB(2).LT.0 )  THEN
*
*........BOTH KEYS ARE NUMERIC
         KAXCMP = KEYA(1) - KEYB(1)
         IF ( KAXCMP .NE. 0 )  RETURN
*
         DO  1  I = 2, KEYA(1)
            KAXCMP = ABS(KEYA(I)) - ABS(KEYB(I))
            IF ( KAXCMP .NE. 0 )  RETURN
 1       CONTINUE
*
      ELSE IF ( KEYA(2).GT.0 .AND. KEYB(2).GT.0 )  THEN
*
*........BOTH KEYS ARE ALPHANUMERIC
         DO  2  I = 2, MIN(KEYA(1),KEYB(1))
            KAXCMP = KEYA(I) - KEYB(I)
            IF ( KAXCMP .NE. 0 )  RETURN
 2       CONTINUE
*
         KAXCMP = KEYA(1) - KEYB(1)
*
      ELSE
*
*........ONE KEY IS NUMERIC AND THE OTHER ALPHANUMERIC
         KAXCMP = KEYA(2)
      ENDIF
*
      END
