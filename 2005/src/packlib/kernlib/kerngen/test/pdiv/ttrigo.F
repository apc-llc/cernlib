*
* $Id: ttrigo.F,v 1.1.1.1 1996/02/15 17:54:56 mclareni Exp $
*
* $Log: ttrigo.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:56  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TTRIGO

#include "kerngent/mkcde.inc"

      DIMENSION    TABLE(9),TABLE2(9)
     +,            AS(9),AC(9),AT(9)
     +,            VASIN(9),VACOS(9),VATG(9)
     +,            PROX(8),PROXMN(8),PROXCK(8),PROXPL(8)

      EQUIVALENCE (B(1),TABLE(1)),(B(11),TABLE2(1))
     +,           (B(21),AS(1)),(B(31),AC(1)),(B(41),AT(1))
     +,           (B(51),VASIN(1)),(B(61),VACOS(1)),(B(71),VATG(1))
     +,           (B(81),PROX(1)),(B(91),PROXMN(1))
     +,           (B(101),PROXPL(1)),(B(111),PROXCK(1))

      PARAMETER (PI = 3.14159 26535 89793)

C-    function ATG from Mathlib here as a statement function
      ATG(RSINA,RCOSA) = ATAN2(RSINA,RCOSA) + (PI-SIGN(PI,RSINA))

      CALL NEWGUY ('ATG-ASIN-ACOS-PROXIM.','TTRIGO  ')

      ZERLEV=ZEROV(3)
      PIBY2= PI/2.
      PIBY4= PI/4.
      CALL UZERO (AT,1,9)

      AK= .1

      DO 10 J=1,9
      TABLE(J)= AK
      AS(J)= SIN(AK)
      AC(J)= COS(AK)
      IF (J.NE.3.AND.J.NE.7)   AT(J)= TAN(AK)
      VASIN(J)= ASIN(AS(J))
      VACOS(J)= ACOS(AC(J))
      VATG(J)= ATG (AS(J),AC(J))

      IF(J.LE.2)          GO TO 9

      IF (J.LE.6)  THEN
          VASIN(J)=  PI - VASIN(J)
        ELSE
          VASIN(J)= 2.*PI + VASIN(J)
        ENDIF

      IF       (J.GE.9)  THEN
          VACOS(J)= 2.*PI + VACOS(J)
        ELSEIF (J.GE.5)  THEN
          VACOS(J)= 2.*PI - VACOS(J)
        ENDIF

    9 VATG(J)= PROXIM(VATG(J),AK)
   10 AK= AK + PIBY4

      CALL MVERIF (1,VATG,TABLE,9)
      CALL MVERIF (2,VASIN,TABLE ,9)
      CALL MVERIF (3,VACOS,TABLE ,9)

      CALL UZERO (TABLE2,1,9)

      DO 20 J=1,9
      IF (J.NE.3.AND.J.NE.7)    TABLE2(J)= AS(J)/AC(J)
   20 CONTINUE

      CALL MVERIF (4,AT,TABLE2,9)

      PROX(1)=ZERLEV

         DO 25 I=2,8
   25 PROX(I)=PROX(I-1)+PIBY2

      CALL UCOPY (PROX,PROXCK,3)
      PROXCK(4)=PROX(1)-PIBY2
      CALL UCOPY (PROXCK,PROXCK(5),4)

         DO 30 I=1,8
      PROXMN(I)=PROXIM (PROX(I),-PIBY4)
   30 PROXPL(I)=PROXIM (PROX(I),PIBY4)

      CALL MVERIF (6,PROXPL,PROXCK,8)

      PROXCK(3)=PROXCK(4)-PIBY2
      CALL UCOPY (PROXCK,PROXCK(5),4)
      CALL MVERIF (5,PROXMN,PROXCK,8)
      RETURN
      END
