*
* $Id: kaxolk.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaxolk.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXOLK(IROOT)
*
*.....OBTAIN A LOCK FOR THE TREE WITH ROOT NUMBER IROOT
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
*.....SEARCH THE LOCK QUEUE FOR ENTRY IROOT AND THE FIRST FREE ENTRY
      L = 0
      DO  1  I = 1, LLOCKQ
         IF ( LOCKI(I).EQ.IROOT .AND. LOCKU(I).EQ.LUNKAF )  THEN
            L = I
            GO TO 2
         ENDIF
         IF ( L.EQ.0 .AND. LOCKN(I).EQ.0 )  L = I
 1    CONTINUE
      IF ( L .EQ. 0 )  GO TO 92
*
*.....LOCK THE TREE IF IT IS NOT ALREADY LOCKED
 2    IF ( LOCKN(L) .EQ. 0 )  THEN
         DO  3  I = 0, MAXTRY
            CALL KAXRD(IROOT,IB,LBLK)
            IF ( IB(9) .EQ. 0 )  THEN
               IB(9) = JOBID
               CALL KAXWRT(IROOT,IB,IB(7)-1)
               GO TO 4
            ENDIF
            CALL KAXAGE(IWAIT)
 3       CONTINUE
         GO TO 91
      ENDIF
*
*.....PUT THE ROOT ID IN THE LOCK QUEUE AND INCREMENT THE LOCK COUNT
 4    LOCKI(L) = IROOT
      LOCKU(L) = LUNKAF
      LOCKN(L) = LOCKN(L) + 1
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
 92   WRITE(MSG,192)
      GO TO 99
*
 99   CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 1911 FORMAT('CRNKA321 KAXOLK: UNABLE TO OBTAIN EXCLUSIVE ACCESS TO',
     +       ' THE KA-FILE ON UNIT ',I3)
 1912 FORMAT('CRNKA322 KAXOLK: UNABLE TO OBTAIN EXCLUSIVE ACCESS TO',
     +       ' MAJOR NAME ''',A,''' ON UNIT ',I3)
 1913 FORMAT('CRNKA323 KAXOLK: UNABLE TO OBTAIN EXCLUSIVE ACCESS TO',
     +       ' TREE ',I11,' ON UNIT ',I3)
 192  FORMAT('CRNKA324 KAXOLK: LOCK QUEUE OVERFLOW')
*
      END
