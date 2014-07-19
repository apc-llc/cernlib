*
* $Id: cqdlin.F,v 1.1.1.1 1996/03/06 10:47:00 mclareni Exp $
*
* $Log: cqdlin.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQDLIN (LGO,JW,NW,MODE)

C-    Write banks of the linear structure starting at LGO
C-    with data words starting at word JW, NW words
C-    MODE = 0  Floating point data
C-           1  Integer
C-           2  Hollerith

#include "test_include/testll.inc"
#include "test_include/testdd.inc"
#include "test_include/testiq.inc"
C--------------    End CDE                             --------------


      CALL CQHEAD (1,'-','DUMP LINEAR STRUCTURE -----.'
     +,                  'JABBBBB','KADR,LADR,NAME,DATA.')

      IF  (MODE-1)           24, 26, 29

   24 CALL CQHEAD (0,'-','-','JAFFFFF','-')
      GO TO 29

   26 CALL CQHEAD (0,'-','-','JAJJJJJ','-')
   29 LAST = NW + 3

      L = LGO
   32 IF (L.EQ.0)            RETURN

      M(1) = LQ(L+2)
      M(2) = L
      M(3) = IQ(L-4)

      DO 34  J=1,NW
   34 M(J+3) = IQ(L+JW+J-1)

      CALL CQDATA (M(1),M(LAST))

      L = LQ(L)
      GO TO 32
      END
*      ==================================================
#include "zebra/qcardl.inc"
