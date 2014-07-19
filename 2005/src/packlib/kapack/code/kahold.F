*
* $Id: kahold.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: kahold.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAHOLD(LUN,MAJNAM,IRC)
*
*.....OBTAIN A LOCK
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax02c.inc"
#include "kapack/kax050.inc"
*
      CHARACTER MAJNAM*(*)
*
*-----------------------------------------------------------------------
*
      CALL KAXINI(LUN)
*
*.....PROCESS THE MAJOR NAME
      IF ( MAJNAM .EQ. ' ' )  THEN
         IROOT = 1
      ELSE
         CALL KAXMAJ(MAJNAM,*11,*21)
         IROOT = KROOT(0)
      ENDIF
*
*.....OBTAIN THE LOCK
      CALL KAXOLK(IROOT)
*
*.....SUCCESSFUL COMPLETION
      IRC = 0
      IF ( MAJNAM .EQ. ' ' )  THEN
         WRITE(MSG,100) LUN
      ELSE
         WRITE(MSG,101) MAJNAM, LUN
      ENDIF
      CALL KAXMSG(LUNLOG,MSG)
      RETURN
*
*.....ERROR PROCESSING
 11   IRC = 1
      WRITE(MSG,111) MAJNAM
      GO TO 99
*
 21   IRC = 2
      WRITE(MSG,121) MAJNAM, LUN
      GO TO 99
*
 99   CALL KAXMSG(LUNERR,MSG)
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 100  FORMAT('CRNKA340 KAHOLD: LOCK HELD FOR MAJOR NAMES ON UNIT ',I3)
 101  FORMAT('CRNKA340 KAHOLD: LOCK HELD FOR MAJOR NAME ''',A,
     +        ''' ON UNIT ',I3)
 111  FORMAT('CRNKA341 KAHOLD: ''',A,''' IS NOT AN ACCEPTABLE MAJOR
     +        NAME')
 121  FORMAT('CRNKA342 KAHOLD: ATTEMPT TO LOCK MAJOR NAME ''',A,
     +       ''' FAILED ON UNIT ',I3,
     +       ', THIS MAJOR NAME DOES NOT EXIST IN THE KA-FILE')
*
      END
