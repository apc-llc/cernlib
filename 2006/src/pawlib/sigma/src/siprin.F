*
* $Id: siprin.F,v 1.1.1.1 1995/12/12 14:36:20 mclareni Exp $
*
* $Log: siprin.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:20  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.19  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIPRIN(KISTRI)
C
C
C    ....................................................
C
C
C         PURPOSE
C            PRINT NEXT N ITEMS ON STACK
C
C         USAGE
C            CALL SIPRIN
C
C         COMM. BLOCKS USED
C            COM1
C            COMVAR USED: NPRINT
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            TRACE
C            NEXT
C            PRIT
C            SIUSTK
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
#include "sigma/sigc.inc"
C
C
C
      CALL SITRAC (' SIPRIN')
C
      CALL SINEXT(N)
      IF(N.GE.1) GOTO 10
C     IF PRINT WITH ZERO ARGS, SKIP TO NEXT PAGE
      WRITE(NPRINT,1000)
 1000 FORMAT(/)
      RETURN
C
 10   CONTINUE
      DO 1 I=1,N
      CALL SIPRIT (N-I,KISTRI)
 1    CONTINUE
C
      DO 2 I=1,N
      CALL SIUSTK
 2    CONTINUE
C
 999  END
