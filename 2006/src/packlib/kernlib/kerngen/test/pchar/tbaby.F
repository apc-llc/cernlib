*
* $Id: tbaby.F,v 1.1.1.1 1996/02/15 17:54:55 mclareni Exp $
*
* $Log: tbaby.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:55  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TBABY

#include "kerngent/mkcde.inc"

      DIMENSION IC(40)
      DIMENSION ICHEK1(9),ICHEK2(12),ICHEK3(12),ICHEK4(6),ICHEK5(6)
      DIMENSION ICHEK6(4)
      DATA  ISEVEN / 4H7   /
      DATA  ICHEK1 / 40,30,0, 4HZ   ,4H7777,4H    , 1,31,41/
      DATA  ICHEK2 / 0,6420,864201,3,  11,16,15,22,  0,4,6,1/
      DATA  ICHEK3 / 90123,0,577,3,  11,12,32,24,  5,0,3,1/
      DATA  ICHEK4 / 10,26,37, 5,6,1/
      DATA  ICHEK5 / 26,1,39,  4,9,1/
      DATA  ICHEK6 / 21,17,  4,3/

C--                PREPARE THE IC-VECTOR

      CALL UCTOH1 ('56789 0123ABCDE86420 1357     77777  Z  ',IC(1),40)
C-                  ....+....1....+....2....+....3....+....4

      CALL NEWGUY ('IULAST-IULOOK-IUNEXT.','TBABY   ')

C--                TEST FOR IULAST
      IA(1)=IULAST (ISEVEN,IC,40)
      IA(2)=IULAST (ISEVEN,IC,35)
      IA(3)=IULAST (ISEVEN,IC(31),5) + IULAST (ISEVEN,IC,0)

C--                TEST FOR IULOOK
      IA(4)=IULOOK (1,IC,36,40)
      IA(5)=IULOOK (4,IC,25,40)
      IA(6)=IULOOK (2,IC,26,30)

C--                TEST FOR IUNEXT
      IA(7)=IUNEXT (IC,1)
      IA(8)=IUNEXT (IC,26)
      IA(9)=IUNEXT (IC,39)

      CALL MVERII (1,IA,ICHEK1,9)

C--                TEST FOR IUBACK - IUEND
      CALL NEWGUY ('IUBACK-IUEND.','TBABY   ')
      IA(1)=IUBACK (IC,6,11)
      IA(5)=IUEND (IA(9))
      IA(2)=IUBACK (IC,17,20)
      IA(6)=IUEND (IA(10))
      IA(3)=IUBACK (IC,15,22)
      IA(7)=IUEND (IA(11))
      IA(4)=IUBACK (IC,23,23)
      IA(8)=IUEND (IA(12))
      CALL MVERII (2,IA,ICHEK2,12)

C--                TEST FOR IUFORW - IUEND
      CALL NEWGUY ('IUFORW-IUEND.','TBABY   ')
      IA(1)=IUFORW (IC,5,11)
      IA(5)=IUEND (IA(9))
      IA(2)=IUFORW (IC,12,14)
      IA(6)=IUEND (IA(10))
      IA(3)=IUFORW (IC,24,31)
      IA(7)=IUEND (IA(11))
      IA(4)=IUFORW (IC,23,23)
      IA(8)=IUEND (IA(12))
      CALL MVERII (3,IA,ICHEK3,12)

C--                TEST FOR ULEFT - IUEND
      CALL NEWGUY ('ULEFT-IUEND.','TBABY   ')
      CALL UCOPY (IC,IA,40)
      CALL ULEFT (IA,5,10)
      IB(1)=IUEND (IB(4))
      CALL ULEFT (IA,20,31)
      IB(2)=IUEND (IB(5))
      CALL ULEFT (IA,36,40)
      IB(3)=IUEND (IB(6))
      CALL MVERII (14,IB,ICHEK4,6)

C-          IC = 40H56789 0123ABCDE86420 1357     77777  Z
C-                  ....+....1....+....2....+....3....+....4
      CALL UCTOH1 ('567890123 ABCDE8642013577      7777Z    ',IB(1),40)
      CALL MVERII (4,IA,IB,40)

C--                TEST FOR URIGHT - IUEND
      CALL NEWGUY ('URIGHT-IUEND.','TBABY   ')
      CALL UCOPY (IC,IA,40)
      CALL URIGHT (IA,21,30)
      IB(1)=IUEND (IB(4))
      CALL URIGHT (IA,1,10)
      IB(2)=IUEND (IB(5))
      CALL URIGHT (IA,36,40)
      IB(3)=IUEND (IB(6))
      CALL MVERII (15,IB,ICHEK5,6)

C-          IC = 40H56789 0123ABCDE86420 1357     77777  Z
C-                  ....+....1....+....2....+....3....+....4
      CALL UCTOH1 (' 567890123ABCDE86420      135777777    Z',IB(1),40)
      CALL MVERII (5,IA,IB,40)

C--                TEST FOR USET - IUEND
      CALL NEWGUY ('USET-IUEND.','TBABY   ')
      CALL UBLANK (IA,1,40)
      CALL USET (1357,IA,21,25)
      IB(1)=IUEND (IB(3))
      CALL USET (86420,IA,18,20)
      IB(2)=IUEND (IB(4))
      CALL MVERII (6,IA(18),IC(18),8)
      CALL MVERII (16,IB,ICHEK6,4)

      RETURN
      END
