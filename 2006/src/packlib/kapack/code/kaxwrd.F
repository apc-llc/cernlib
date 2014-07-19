*
* $Id: kaxwrd.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaxwrd.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXWRD(STRING,LOC,LENGTH,*)
*
*.....FIND THE LOCATION AND LENGTH OF A WORD DELIMITED BY BLANKS
*
      CHARACTER STRING*(*)
*
*-----------------------------------------------------------------------
*
      DO  1  LOC = 1, LEN(STRING)
         IF ( STRING(LOC:LOC) .NE. ' ' )  THEN
            LENGTH = INDEX(STRING(LOC:),' ') - 1
            IF ( LENGTH .LT. 0 )  LENGTH = LEN(STRING) - LOC + 1
            RETURN
         ENDIF
 1    CONTINUE
*
      RETURN 1
*
      END
