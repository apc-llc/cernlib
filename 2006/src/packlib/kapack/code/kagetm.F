*
* $Id: kagetm.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: kagetm.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAGETM(LUN,MAJNAM,MODE,LDEF,LMAX,IRC)
*
*.....RETRIEVE A MAJOR ENTRY FROM A KA-FILE
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax02c.inc"
#include "kapack/kax050.inc"
#include "kapack/kax0a0.inc"
*
      CHARACTER MAJNAM*(*), MODE*(*)
*
*-----------------------------------------------------------------------
*
      CALL KAXINI(LUN)
*
*.....LOCK THE MAJOR TREE IF THE MODE IS UPDATE
      IF ( MODE .EQ. 'UPDATE')  THEN
         CALL KAXOLK(1)
      ELSE IF ( MODE .NE. 'READ' )  THEN
         GO TO 11
      ENDIF
*
*.....PROCESS THE MAJOR NAME
      CALL KAXMAJ(MAJNAM,*12,*21)
*
*.....STORE THE DEFAULT AND MAXIMUM LENGTHS FOR THE CALLER
      LDEF = KLDEF(0)
      LMAX = KLMAX(0)
*
*.....SUCCESSFUL COMPLETION
      IRC = 0
      RETURN
*
*.....ERROR PROCESSING
 11   IRC = 1
      WRITE(MSG,111) MODE
      GO TO 99
*
 12   IRC = 1
      WRITE(MSG,112) MAJNAM
      IF ( MODE .EQ. 'UPDATE' )  CALL KAXRLK(1)
      GO TO 99
*
 21   IRC = 2
      WRITE(MSG,121) MAJNAM, LUN
      IF ( MODE .EQ. 'UPDATE' )  CALL KAXRLK(1)
      GO TO 99
*
 99   CALL KAXMSG(LUNERR,MSG)
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 111  FORMAT('CRNKA061 KAGETM: ''',A,''' IS NOT AN ACCEPTABLE VALUE FOR
     +        THE THIRD ARGUMENT, (MODE), IT MUST BE EITHER ''READ'' OR
     +        ''UPDATE''')
 112  FORMAT('CRNKA062 KAGETM: ''',A,''' IS NOT AN ACCEPTABLE MAJOR
     +        NAME')
 121  FORMAT('CRNKA063 KAGETM: RETRIEVAL OF MAJOR NAME ''',A,
     +       ''' FAILED ON UNIT ',I3,
     +       ', THIS MAJOR NAME DOES NOT EXIST IN THE KA-FILE')
*
      END
