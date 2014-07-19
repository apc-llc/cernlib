*
* $Id: kaxls1.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaxls1.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXLS1(MAJOR)
*
*.....LIST THE MINOR NAMES FOR THE SPECIFIED MAJOR NAME
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
*
      CHARACTER MAJOR*(*), MINOR*(MAXNAM)
*
*-----------------------------------------------------------------------
*
         NMINOR = 0
         MINOR  = ' '
*
 1       CALL KASEQ1(LUNKAF,MAJOR,MINOR,NWORDS,IRC)
         IF ( IRC .EQ. 0 )  THEN
            NMINOR = NMINOR + 1
            NC     = INDEX(MINOR,' ') - 1
            WRITE(LUNLOG,100) NMINOR, NWORDS, MINOR(1:NC)
            GO TO 1
         ELSE IF ( IRC .EQ. 3 )  THEN
            IF ( NMINOR .EQ. 0 )  WRITE(LUNLOG,101)
         ENDIF
*
*.....SUCCESSFUL COMPLETION
      RETURN
*
 100  FORMAT('           ',I5,'. LENGTH ',I5,', MINOR NAME: ',A)
 101  FORMAT(' ',13X,'THERE ARE NO RECORDS FOR THIS MAJOR NAME')
*
      END
