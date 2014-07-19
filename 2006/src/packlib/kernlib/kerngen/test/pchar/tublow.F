*
* $Id: tublow.F,v 1.1.1.1 1996/02/15 17:54:55 mclareni Exp $
*
* $Log: tublow.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:55  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TUBLOW

#include "kerngent/mkcde.inc"

      COMMON /SLATE/ ISLAT(40)

      INTEGER      CHECK(10), CHECK4(6), CHKA1(4)
      INTEGER      TINF(4), TINF2

      DATA CHECK/4H 012,4H3456,4H789A,4HBCDE,4HFGHI,4HJKLM,4HNOPQ,
     +4HRSTU,4HVWXY,4HZ   /
      DATA NCH/37/

      DATA  CHECK4 /4H 012, 4H3456, 4H789A, 4HBCDE, 4HFGHI, 4HJ    /
      DATA  CHKA1  /4HA   , 4HB   , 4HC   , 4HD     /
      DATA  IABCD  /4HABCD /

      DATA   TINF /1000, 4HUBLW, 40, 4HCHAR/,   TINF2 /4HUBCH/

      CALL UZERO(A,1,NCH)
      CALL NEWGUY ('UBLOW-UBUNCH.','TUBLOW  ')

      CALL UBUNCH (IBCD,A,NCH)
      CALL UBUNCH (IBCD(12),A,0)
      NW= (NCH-1)/IQCHAW +1
      CALL MVERII (1,A,CHECK,NW)

      NC2=(NW-1)*IQCHAW
      NCH2=NCH-NC2
      CALL UBUNCH (IBCD(NC2+1),A(NW+1),NCH2)
      CALL MVERII (2,A(NW+1),CHECK(NW),1)

      CALL UCOPY (IBCD,IB(101),NCH-1)
      IB(NCH+100)=NCH
      IA(NCH+100)=NCH
      CALL UBLOW (CHECK,IA(101),NCH-1)
      CALL UBLOW (CHECK,IA(121),0)
      CALL MVERII (3,IA(101),IB(101),NCH)

      CALL UCOPY (IBCD(NC2+1),IB(101),NCH2)
      CALL UBLOW (CHECK(NW),IA(101),NCH2)
      CALL MVERII (4,IA(101),IB(101),NCH)

      CALL UBLOW (IABCD,IA(101),4)
      CALL MVERII (5,IA(101),CHKA1,4)

      CALL UCOPY (IBCD,B,27)

      DO 27 J=1,27
      NCH = 28 - J
      NW  = (NCH-1)/IQCHAW + 1
      CALL VZERO  (A,NCH+1)
      IA(NCH+2)  = IBCD(3)
      IB(NCH+2)  = IBCD(3)
      IB(NCH+1)  = IBCD(1)
      IA(NW+101) = IBCD(1)
      CALL UBUNCH (IBCD,A(101),NCH)
      CALL UBLOW  (A(101),A(1),NCH+1)
      CALL MVERII (J+10,A,B,NCH+2)
   27 CONTINUE

C----              UTRANS FOR   A1 <---> A4

      CALL NEWGUY ('UTRANS FOR A1.','TUBLOW  ')
      CALL UCOPY (IBCD,  B,    32)
      CALL UCOPY (CHECK4,B(51), 6)

      DO 34 JJ=1,5
      JH  = 6 - JJ
      NCH = 4*JH
      CALL VZERO (A,JH+50)

      IA(JH+51) = IBCD(3)
      IB(JH+51) = IBCD(3)
      CALL UTRANS (IBCD,A(51),NCH, 1,4)
      IA(57) = ISLAT(1)
      IA(58) = ISLAT(2)
      IB(57) = NCH
      IB(58) = JH
      CALL MVERII (JJ,A(51),B(51),8)

      IB(JH+51) = IBCD(NCH+1)
      CALL UTRANS (IBCD,A(51),NCH+1, 1,4)
      IA(57) = ISLAT(1)
      IA(58) = ISLAT(2)
      IB(57) = NCH+ 1
      IB(58) = JH + 1
      CALL MVERII (JJ+10,A(51),B(51),8)

      IA(NCH+3) = IB(NCH+3)
      IB(NCH+2) = IBCD(1)
      CALL UTRANS (B(51),A,NCH+2, 4,1)
      IA(NCH+4) = ISLAT(1)
      IA(NCH+5) = ISLAT(2)
      IB(NCH+4) = JH + 1
      IB(NCH+5) = NCH+ 2
      CALL MVERII (JJ+20,A,B,NCH+5)

      IA(NCH+1) = IBCD(3)
      IB(NCH+1) = IBCD(3)
      CALL UTRANS (B(51),A,NCH, 4,1)
      IA(NCH+2) = ISLAT(1)
      IA(NCH+3) = ISLAT(2)
      IB(NCH+2) = JH
      IB(NCH+3) = NCH
      CALL MVERII (JJ+30,A,B,NCH+3)
   34 CONTINUE

C----              UTRANS FOR   A4 <---> AX

      CALL NEWGUY ('UTRANS FOR A4.','TUBLOW  ')
      CALL UCOPY  (IBCD,B(201),27)

      DO 44  JJ=1,22
      NCH = 23 - JJ
      NW4 = (NCH-1)/4 + 1
      NW  = (NCH-1)/IQCHAW + 1

      IB(NCH+201) = IBCD(1)
      CALL UTRANS (B(201), B,      NCH+1, 1,4)
      CALL UBUNCH (B(201), B(21),  NCH+1)

      IB(NCH+201) = IBCD(3)
      CALL UTRANS (B(201), B(101), NCH+1, 1,4)
      CALL UBUNCH (B(201), B(121), NCH+1)
      CALL VZERO (A,NW+20)

      IA(NW+21) = NCH
      IB(NW+21) = NCH
      IB(NW+22) = NW4
      IB(NW+23) = NW
      CALL UTRANS (B(101),A(21),NCH, 4,99)
      IA(NW+22) = ISLAT(1)
      IA(NW+23) = ISLAT(2)
      CALL MVERII (JJ,A(21),B(21),NW+3)

      IA(NW4+1) = NCH
      IB(NW4+1) = NCH
      IB(NW4+2) = NW4
      IB(NW4+3) = NW
      CALL UTRANS (B(121),A,NCH, 99,4)
      IA(NW4+2) = ISLAT(2)
      IA(NW4+3) = ISLAT(1)
      CALL MVERII (JJ+50,A,B,NW4+3)
   44 CONTINUE

C--      TIMING
      IF (ITIMES.EQ.0) RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      CALL UBUNCH(IBCD,A,40)
      CALL TIMED (TIMERD)

      DO 80 J=1,NTIMES
   80 CALL UBLOW(A,B,40)

      CALL TIMING (TINF)
      TINF(2)= TINF2
      CALL TIMED (TIMERD)

      DO 81 J=1,NTIMES
   81 CALL UBUNCH (B,A,40)

      CALL TIMING (TINF)
      RETURN

      END
