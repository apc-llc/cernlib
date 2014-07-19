*
* $Id: tuopt.F,v 1.1.1.1 1996/02/15 17:54:57 mclareni Exp $
*
* $Log: tuopt.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:57  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TUOPT

#include "kerngent/mkcde.inc"

      EQUIVALENCE (IOPT, IA(101))

C--                THE TEST VECTOR
C--      0 1 2 3 4 5 6 7 8 9 A B C D E F G H I J K L M N O P Q R S T
C--      - -       -     - - - -
      CALL UZERO (IB,1,100)
      IB(1)=1
      IB(2)=1
      IB(6)=1
      IB(9)=1
      IB(10)=1
      IB(11)=1
      IB(12)=1

      CALL NEWGUY ('UOPT.','TUOPT   ')

      CALL UBUNCH (IBCD(2),IA,30)
      CALL UCTOH  ('8910AB5XY456PQ',IA(401),99,30)
      CALL UOPT   (IA(401),IA,IOPT,30)
      CALL MVERII (1,IOPT,IB,30)
      RETURN
      END
