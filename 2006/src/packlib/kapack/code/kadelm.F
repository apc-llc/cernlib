*
* $Id: kadelm.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: kadelm.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KADELM(LUN,MAJNAM,IRC)
*
*.....DELETE A MAJOR ENTRY FROM A KA-FILE
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax02c.inc"
#include "kapack/kax050.inc"
#include "kapack/kax0a0.inc"
#include "kapack/kax0b0.inc"
*
      CHARACTER MAJNAM*(*)
*
*-----------------------------------------------------------------------
*
      CALL KAXINI(LUN)
      IF ( .NOT. MODIFY )  GO TO 21
*
*.....PROCESS THE MAJOR NAME
      CALL KAXMAJ(MAJNAM,*11,*31)
*
*.....ENSURE THERE ARE NO ASSOCIATED RECORDS
      CALL KAXRD(KROOT(0),IB,NBCW+NRCW+2)
      IF ( IB(5).NE.1 .OR. IB(IB(6)+NRCW+1).NE.MAXKEY )  GO TO 41
*
*.....INVALIDATE THE MAJOR STACK ENTRIES
      KUNIT(0) = -1
      KUNIT(1) = -1
*
*.....DELETE THE MAJOR ENTRY AND FREE THE ROOT BLOCK OF THE MINOR TREE
* . . (NOTE THAT BLOCK 1 MUST BE LOCKED THROUGHOUT THIS PROCESS)
      CALL KAXOLK(1)
      CALL KAXDEL(1,MAJKEY,*31)
      CALL KAXFBK(KROOT(0))
      CALL KAXRLK(1)
*
*.....SUCCESSFUL COMPLETION
      IRC = 0
      WRITE(MSG,100) MAJNAM, LUN
      CALL KAXMSG(LUNLOG,MSG)
      RETURN
*
*.....ERROR PROCESSING
 11   IRC = 1
      WRITE(MSG,111) MAJNAM
      GO TO 99
*
 21   IRC = 2
      WRITE(MSG,121) LUN
      GO TO 99
*
 31   IRC = 3
      CALL KAXRLK(1)
      WRITE(MSG,131) MAJNAM, LUN
      GO TO 99
*
 41   IRC = 4
      WRITE(MSG,141) MAJNAM, LUN
      GO TO 99
*
 99   CALL KAXMSG(LUNERR,MSG)
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 100  FORMAT('CRNKA040 KADELM: MAJOR NAME ''',A,''' HAS BEEN DELETED
     +        FROM THE KA-FILE ON UNIT ',I3)
 111  FORMAT('CRNKA041 KADELM: ''',A,''' IS NOT AN ACCEPTABLE MAJOR
     +        NAME')
 121  FORMAT('CRNKA042 KADELM: MODIFICATION ATTEMPTED ON UNIT ',I3,
     +       ' WITH MODIFY=NO SET')
 131  FORMAT('CRNKA043 KADELM: DELETION OF MAJOR NAME ''',A,
     +       ''' FAILED ON UNIT ',I3,', THIS MAJOR NAME DOES NOT EXIST
     +        IN THE KA-FILE')
 141  FORMAT('CRNKA044 KADELM: DELETION OF MAJOR NAME ''',A,
     +       ''' FAILED ON UNIT ',I3,', THIS MAJOR NAME STILL HAS
     +        ASSOCIATED RECORDS')
*
      END
