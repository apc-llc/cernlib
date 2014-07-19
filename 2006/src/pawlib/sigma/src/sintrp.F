*
* $Id: sintrp.F,v 1.1.1.1 1995/12/12 14:36:19 mclareni Exp $
*
* $Log: sintrp.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:19  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.19  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SINTRP
C
C    ..................................................
C
C         PURPOSE
C            TO INTERPRETE FAMILY CODES AND DEFINE KLASS
C
C         USAGE
C            CALL SINTRP
C
C         COMM. BLOCKS USED
C            COM1
C            COMVAR USED: KLASS
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            TRACE
C            SIFAM0
C            SIFAM1
C            SIFAM2
C            SIFAM3
C            SIFAM4
C            SIFAM5
C            SIFAM6
C            SIFAM7
C            SINERR
C
C...  PAW VERSION  ...  MAY 1988
C
C    ..................................................
C
#include "sigma/sigc.inc"
C
C
      CALL SITRAC (' SINTRP  ')
C
 100  CONTINUE
C
C
      IF(IERRNO.NE.0) RETURN
C
      CALL SINEXT(ICODE)
      IF(ICODE.EQ.777) GOTO 101
      IFA=ICODE/100+1
      IF(IFA.LT.1.OR.IFA.GT.8) CALL SINERR(6)
      IF(IERRNO.NE.0) GOTO 999
      GO TO(1,2,3,4,5,6,7,8) IFA
C
C     FAMILY 000
 1    KLASS=ICODE
      CALL SIFAM0
      GO TO 100
C
C     FAMILY 100
 2    KLASS=ICODE-100
      CALL SIFAM1
      GO TO 100
C
C     FAMILY 200
 3    KLASS=ICODE-200
      CALL SIFAM2
      GO TO 100
C
C     FAMILY 300
 4    KLASS=ICODE-300
      CALL SIFAM3
      GO TO 100
C
C     FAMILY 400
 5    KLASS=ICODE-400
      CALL SIFAM4
      GO TO 100
C
C     FAMILY 500
 6    KLASS=ICODE-500
      CALL SIFAM5
      GO TO 100
C
C     FAMILY 600
 7    KLASS=ICODE-600
      CALL SIFAM6
      GO TO 100
C
C     FAMILY 700
 8    KLASS=ICODE-700
      CALL SIFAM7
      GO TO 100
C
 101  CONTINUE
         IPOINT=1
 999  END
