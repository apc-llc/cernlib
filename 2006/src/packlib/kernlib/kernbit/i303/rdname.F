*
* $Id: rdname.F,v 1.2 2000/03/31 15:17:05 mclareni Exp $
*
* $Log: rdname.F,v $
* Revision 1.2  2000/03/31 15:17:05  mclareni
* Remove spurious character in column 74 to help Absoft f90 extended format
*
* Revision 1.1.1.1  1996/02/15 17:47:46  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE RDNAME(NAME)
C** RDNAME IS CALLED WHEN FIRST CHAR OF A WORD IS A LETTER
C******************************************************************
      CHARACTER*(*) NAME
#include "i303cd.inc"
      LOGICAL RDIGI,RALPH

      IBEG=IBPTR
    1 IBPTR=IBPTR+1
      IF (IBPTR .GT. 80) GO TO 2
      CURNT=BUFFER(IBPTR:IBPTR)
      IF (RALPH()) GO TO 1
C    *** IN CASE ALPHANUMERICAL NAMES ARE CONSIDERED
      IF ( (MODE .EQ. 2) .AND. RDIGI() ) GO TO 1
    2 CONTINUE
      NAME=BUFFER(IBEG:(IBPTR-1))
C    *** CONVERT NAME TO UPPER CASE
      CALL RDUPST(NAME)
      IF (LEN(NAME) .LT. IBPTR-IBEG) THEN
         PRINT*,
     - '**ERROR IN RDWORD: NAME READ IS TOO LONG. IT IS TRUNCATED TO: ' 
     -   ,NAME
      ENDIF
      END
