*
* $Id: kalen.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: kalen.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KALEN(LUN,MAJNAM,MINNAM,LDEF,LMAX,NWORDS,IRC)
*
*.....RETRIEVE THE LENGTH ATTRIBUTES OF A RECORD
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
*
*.....PROCESS THE MAJOR NAME
      CALL KAXMAJ(MAJNAM,*11,*21)
*
*.....CONVERT THE MINOR NAME TO A KEY
      CALL KAXNAM(MINNAM,MINKEY,*12)
*
*.....STORE THE DEFAULT AND MAXIMUM LENGTHS FOR THE CALLER
      LDEF = KLDEF(0)
      LMAX = KLMAX(0)
*
*.....LOCATE THE RECORD
      CALL KAXGET(KROOT(0),MINKEY,1,LOC,*31)
*
*.....STORE THE RECORD LENGTH FOR THE CALLER
      NWORDS = IA(LOC) - NRCW - IA(LOC+NRCW)
*
*.....SUCCESSFUL COMPLETION
      IRC = 0
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
      IF ( RETURN )  RETURN
      WRITE(MSG,121) MAJNAM, LUN
      GO TO 99
*
 31   IRC = 3
      IF ( RETURN )  RETURN
      WRITE(MSG,131) MAJNAM, MINNAM, LUN
      GO TO 99
*
 99   CALL KAXMSG(LUNERR,MSG)
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 111  FORMAT('CRNKA311 KALEN: ''',A,''' IS NOT AN ACCEPTABLE MAJOR
     +        NAME')
 112  FORMAT('CRNKA312 KALEN: ''',A,''' IS NOT AN ACCEPTABLE MINOR
     +        NAME')
 121  FORMAT('CRNKA313 KALEN: THE MAJOR NAME ''',A,''' DOES NOT EXIST
     +        IN THE KA-FILE ON UNIT ',I3)
 131  FORMAT('CRNKA314 KALEN: THE RECORD ''',A,' ',A,''' DOES NOT EXIST
     +        IN THE KA-FILE ON UNIT ',I3)
*
      END
