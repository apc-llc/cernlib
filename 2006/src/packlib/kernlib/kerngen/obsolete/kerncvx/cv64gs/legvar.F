*
* $Id: legvar.F,v 1.1.1.1 1996/02/15 17:52:16 mclareni Exp $
*
* $Log: legvar.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:16  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION LEGVAR (IWORD)
C
C CERN PROGLIB#         LEGVAR          .VERSION KERNCVX  1.04  890626
C ORIG. 22/06/89  JZ
C
C-    CONVEX version of the CDC function LEGVAR
C-    Returns zero for normal floating point numbers
C-    Returns 1 for maximum exponent (kind of overflow)
C-    Returns 2 for reserved operand


      LEGVAR = 0

      NN = ISHFT (IWORD, -52)
      IF (NN.EQ.'4000'O)           GO TO 27
      NN = NN .AND. '3777'O
      IF  (NN .NE.  '3777'O)       RETURN
      LEGVAR = 1
      RETURN

   27 LEGVAR = 2
      RETURN
      END
