*
* $Id: kaxrlk.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaxrlk.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXRLK(IROOT)
*
*.....RELEASE A LOCK FOR THE TREE WITH ROOT NUMBER IROOT
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax02c.inc"
#include "kapack/kax050.inc"
#include "kapack/kax070.inc"
#include "kapack/kax0b0.inc"
*
*-----------------------------------------------------------------------
*
*.....SEARCH THE LOCK QUEUE FOR ENTRY IROOT
      DO  1  I = 1, LLOCKQ
         IF ( LOCKI(I).EQ.IROOT .AND. LOCKU(I).EQ.LUNKAF )  THEN
            L = I
            GO TO 2
         ENDIF
 1    CONTINUE
      GO TO 91
*
*.....DECREMENT THE LOCK COUNT
 2    IF ( LOCKN(L) .EQ. 0 )  GO TO 91
      LOCKN(L) = LOCKN(L) - 1
*
*.....DELETE THE LOCK IF THE COUNT HAS REACHED ZERO
      IF ( LOCKN(L) .EQ. 0 )  THEN
         CALL KAXRD(IROOT,IB,LBLK)
         IF ( IB(9) .NE. JOBID )  GO TO 92
         IB(9) = 0
         CALL KAXWRT(IROOT,IB,IB(7)-1)
      ENDIF
*
*.....SUCCESSFUL COMPLETION
      RETURN
*
*.....ERROR PROCESSING
 91   IF ( IROOT .EQ. 1 )  THEN
         WRITE(MSG,1911) LUNKAF
      ELSE IF ( IROOT .EQ. KROOT(0) )  THEN
         WRITE(MSG,1912) KMAJOR(0), LUNKAF
      ELSE
         WRITE(MSG,1913) IROOT, LUNKAF
      ENDIF
      GO TO 99
*
 92   WRITE(MSG,192) IB(9), LUNKAF
      GO TO 99
*
 99   CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 1911 FORMAT('CRNKA331 KAXRLK: KAPACK SYSTEM ERROR, INVALID REQUEST TO',
     +       ' RELEASE THE KA-FILE ON UNIT ',I3)
 1912 FORMAT('CRNKA332 KAXRLK: KAPACK SYSTEM ERROR, INVALID REQUEST TO',
     +       ' RELEASE MAJOR NAME ''',A,''' ON UNIT ',I3)
 1913 FORMAT('CRNKA333 KAXRLK: KAPACK SYSTEM ERROR, INVALID REQUEST TO',
     +       ' RELEASE TREE ',I11,' ON UNIT ',I3)
 192  FORMAT('CRNKA334 KAXRLK: ACCESS CONFLICT WITH JOB ',I11,
     +       ', DATA MAY HAVE BEEN LOST WHILE UPDATING THE KA-FILE ON ',
     +       ' UNIT ',I3)
*
      END
