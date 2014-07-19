*
* $Id: kadel.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: kadel.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KADEL(LUN,MAJNAM,MINNAM,IRC)
*
*.....DELETE A RECORD FROM A KA-FILE
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax050.inc"
#include "kapack/kax0a0.inc"
*
      CHARACTER MAJNAM*(*), MINNAM*(*)
*
*-----------------------------------------------------------------------
*
      CALL KAXINI(LUN)
      IF ( .NOT. MODIFY )  GO TO 21
*
*.....PROCESS THE MAJOR NAME
      CALL KAXMAJ(MAJNAM,*11,*31)
*
*.....CONVERT THE MINOR NAME TO A KEY
      CALL KAXNAM(MINNAM,MINKEY,*12)
*
*.....DELETE THE RECORD
      CALL KAXDEL(KROOT(0),MINKEY,*41)
*
*.....SUCCESSFUL COMPLETION
      IRC = 0
      WRITE(MSG,100) MAJNAM, MINNAM, LUN
      CALL KAXMSG(LUNLOG,MSG)
      RETURN
*
*.....ERROR PROCESSING
 11   IRC = 1
      WRITE(MSG,111) MAJNAM
      GO TO 99
*
 12   IRC = 1
      WRITE(MSG,112) MINNAM
      GO TO 99
*
 21   IRC = 2
      WRITE(MSG,121) LUN
      GO TO 99
*
 31   IRC = 3
      WRITE(MSG,131) MAJNAM, MINNAM, LUN
      GO TO 99
*
 41   IRC = 4
      WRITE(MSG,141) MAJNAM, MINNAM, LUN
      GO TO 99
*
 99   CALL KAXMSG(LUNERR,MSG)
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 100  FORMAT('CRNKA030 KADEL: ''',A,' ',A,
     +       ''' HAS BEEN DELETED FROM THE KA-FILE ON UNIT ',I3)
 111  FORMAT('CRNKA031 KADEL: ''',A,''' IS NOT AN ACCEPTABLE MAJOR
     +        NAME')
 112  FORMAT('CRNKA032 KADEL: ''',A,''' IS NOT AN ACCEPTABLE MINOR
     +        NAME')
 121  FORMAT('CRNKA033 KADEL: MODIFICATION ATTEMPTED ON UNIT ',I3,
     +       ' WITH MODIFY=NO SET')
 131  FORMAT('CRNKA034 KADEL: DELETION OF ''',A,' ',A,''' FAILED
     +        ON UNIT ',I3,', THE MAJOR NAME DOES NOT EXIST IN THE
     +        KA-FILE')
 141  FORMAT('CRNKA035 KADEL: DELETION OF ''',A,' ',A,''' FAILED
     +        ON UNIT ',I3,', THIS RECORD DOES NOT EXIST IN THE
     +        KA-FILE')
*
      END
