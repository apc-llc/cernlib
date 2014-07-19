*
* $Id: xot123.F,v 1.1.1.1 1996/02/15 17:53:28 mclareni Exp $
*
* $Log: xot123.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:28  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XOT123 (LUN,IPAR,A,N)
C
C
C-    ROUTINE COMMON TO THE BASIC AND THE EXTENDED X-PACKAGE.
C
      DIMENSION A(N)
C
C
      IF (IPAR) 99,20,10
   10 WRITE (LUN) A
      RETURN
C
   20 WRITE (LUN,1000) A
   99 RETURN
C
 1000 FORMAT (13A10,A6)                                                 A10
C1000 FORMAT (17A8)                                                     A8
C1000 FORMAT (22A6)                                                     A6
C1000 FORMAT (27A5)                                                     A5
C1000 FORMAT (33A4)                                                     A4
C
C
C-  USING THE 'SHORT LIST' FORM OF I/O IS 10 TIMES FASTER WITH SIPROS
C-  THAN USING THE 'LONG LIST' FORM    WRITE (LUN) (A(J),J=JA,JE)
      END
