*
* $Id: tvec.F,v 1.1.1.1 1996/02/15 17:54:59 mclareni Exp $
*
* $Log: tvec.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:59  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TVEC

#include "kerngent/mkcde.inc"


C*A*   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22    24 25
C*B*  15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0 -1 -2 -3 -4 -5 -6    -8 -9

      CALL NEWGUY ('VADD-VSUB-VMUL-VBIAS-VSCALE-VLINE-VUNIT.',
     +             'TVEC    ')
      ZERLEV = ZEROV(2)

      ALPHA=5.
      BETA=2.
      V=0.

      J=15
         DO 10 I=1,25
      A(I)=I
      B(I)=J
      B(I+100)=I+J
      B(I+150)=J-I
      B(I+200)=I*J
      B(I+250)=ALPHA+B(I)
      B(I+300)=BETA*B(I)
      B(I+350)=ALPHA*A(I) + B(I+300)
      V=V+B(I)*B(I)
   10 J=J-1

      V1=1./SQRT (V)
      CALL VZERO (B(26),25)
         DO 15 I=1,25
   15 B(I+400)=B(I)*V1

      CALL VADD (A,B,A(101),25)
      CALL VADD (A(5),B,A(101),0)

      CALL VSUB (B,A,A(151),25)
      CALL VSUB (A,B,A(151),0)

      CALL VMUL (A,B,A(201),25)
      CALL VMUL (A(5),B,A(201),0)

      CALL VBIAS (B,ALPHA,A(251),25)
      CALL VBIAS (A,BETA,A(251),0)

      CALL VSCALE (B,BETA,A(301),25)
      CALL VSCALE (A,ALPHA,A(301),0)

      CALL VLINE (A,ALPHA,B,BETA,A(351),25)
      CALL VLINE (A,BETA,B,ALPHA,A(351),0)

      CALL VUNIT (B,A(401),25)
      CALL VUNIT (A,A(401),0)

      LOC=101
      DO 20 IT=1,6
      CALL MVERII (IT,A(LOC),B(LOC),25)
   20 LOC=LOC+50

      CALL MVERIF (7,A(LOC),B(LOC),25)

      CALL NEWGUY ('VSUM-VASUM-VMAX-VMAXA-VMIN-VMINA-+++.','TVEC    ')
      CALL NEWGUY ('VEXCUM-LVMAX-LVMAXA-LVMIN-LVMINA.','TVEC    ')
      CALL VZERO (B(26),25)

         DO 25 I=1,25
      B(26)=B(26)+A(I)
      B(27)=B(27)+B(I)
   25 B(29)=B(29)+ABS (B(I))
      B(28)=B(26)
      B(30)=B(28)
      B(31)=9.
      B(32)=B(28)
      B(33)=9.
      B(34)=1.
      B(35)=-9.
      B(36)=1.
      B(37)=1.
      B(38)=B(35)
      B(39)=25.
      B(40)=10.+B(26)+B(27)
      IB(41)=26
      IB(42)=1
      IB(43)=26
      IB(44)=18
      IB(45)=1
      IB(46)=25
      IB(47)=1
      IB(48)=1

      A(26)=VSUM (A,25) + VSUM (A,0)                                        325.
      A(27)=VSUM (B,25) + VSUM (B,0)                                         75.

      A(28)=VASUM (A,25) + VASUM (A,0)                                      325.
      A(29)=VASUM (B,25) + VASUM (B,0)                                      165.

      A(30)=VMAX (A,29) + VMAX (A,0)                                        325.
      A(31)=VMAX (B(7),19) + VMAX(B,0)                                        9.

      A(32)=VMAXA (A,31) + VMAXA (A,0)                                      325.
      A(33)=VMAXA (B(8),18) + VMAXA(B,0)                                      9.

      A(34)=VMIN (A,33) + VMIN(A,0)                                           1.
      A(35)=VMIN (B,34) + VMIN (B,0)

      A(36)=VMINA (A,35) + VMINA (A,0)                                        1.
      A(37)=VMINA (B(17),10) + VMINA (B,0)                                    1.

      CALL VFILL (A(38),3,10.)                                             3*10.
      CALL VEXCUM (A,A(38),25)
      CALL VEXCUM (B,A(38),25)
      CALL VEXCUM (A,A(38),0)

      IA(41)=LVMAX (A,29) + LVMAX(A,0)                                        26
      IA(42)=LVMAX (B(7),19) + LVMAX (B,0)                                     1

      IA(43)=LVMAXA (A,31) + LVMAXA (A,0)                                     26
      IA(44)=LVMAXA (B(8),18) + LVMAXA (B,0)                                  18

      IA(45)=LVMIN (A,33) + LVMIN (A,0)                                        1
      IA(46)=LVMIN (B,34) + LVMIN (B,0)                                       25

      IA(47)=LVMINA (A,35) + LVMINA (A,0)                                      1
      IA(48)=LVMINA (B(17),10) + LVMINA (B,0)                                  1

      CALL MVERII (10,A(26),B(26),23)

      CALL NEWGUY ('VMOD-DOTI-VDOT-VDOTN-VDOTN2-VDIST-VDIST2.',
     +             'TVEC    ')

      CALL VZERO (B(51),50)

         DO 30 I=1,3
      B(51)=B(51) + A(I)*A(I)
      B(52)=B(52) + B(I)*B(I)
      B(54)=B(54) + A(I+3)*A(I+3)
      B(55)=B(55) + B(I+3)*B(I+3)
      B(57)=B(57) + B(I+150)*B(I+150)
      B(58)=B(58) + B(I+166)*B(I+166)
      B(59)=B(59) + B(I+200)
      B(60)=B(60) + B(I+216)
      B(67)=B(67) + B(I+204)
      B(68)=B(68) + B(I+220)
      B(81)=B(81) + A(I+16)*A(I+16)
      B(82)=B(82) + A(I+19)*A(I+19)
   30 CONTINUE

      B(91)=SQRT (B(51)+B(54)+A(7)*A(7))
      B(92)=SQRT (B(52)+B(55)+B(7)*B(7))
      B(93)=SQRT (B(81)+B(82)+A(23)*A(23))
      B(81)=SQRT (B(81))

      B(54)=SQRT (B(54)+B(51))
      B(55)=SQRT (B(55)+B(52))
      B(56)=B(54)

      B(51)=SQRT (B(51))
      B(52)=SQRT (B(52))
      B(53)=B(51)

      B(57)=SQRT (B(57))
      B(58)=SQRT (B(58))

      B(61)=B(59) + B(204)
      B(62)=B(60) + B(220)

      B(63)=B(59) - B(204)
      B(64)=B(60) - B(220)

      B(65)=B(59)/(B(51)*B(52))
      B(66)=B(60)/(B(81)*B(51))

      B(67)=B(67)+B(61)
      B(68)=B(68)+B(62)

      B(69)=B(67)/(B(91)*B(92))
      B(70)=B(68)/(B(91)*B(93))
      B(71)=B(69)**2
      B(72)=B(70)**2

      A(51)= VMOD(A,3)
      A(52)= VMOD(B,3)
      A(53)= VMOD(B(17),3)

      A(54)=VMOD (A,6) + VMOD (A,0)
      A(55)=VMOD (B,6) + VMOD (A,0)
      A(56)=VMOD (B(17),6) + VMOD (B,0)

      A(57)= VDIST (A,B,3)    +  VDIST (A,B,0)
      A(58)= SQRT (VDIST2(A(17),B(17),3))  + VDIST2 (A,B,0)

      A(63)=DOTI (A,B)
      A(64)=DOTI (A(17),B(17))

C-    ELEMENTS 59 - 60 - 61 - 62 - 65 - 66 ARE DUMMY FROM VS 2.04 ON.
C-    PREVIOUS VALUES     ---------------------
C     A(57)=DIST (A,$)
C     A(58)=DIST (A(17),B(17))
C     A(59)=DOT (A,B)
C     A(60)=DOT (A(17),B(17))
C     A(61)=DOT4 (A,B)
C     A(62)=DOT4 (A(17),B(17))
C     A(65)=DOTNOR (A,B)
C     A(66)=DOTNOR (A(17),B(17))

C-    DUMMY VALUES        ---------------------
      A(59)=B(59)
      A(60)=B(60)
      A(61)=B(61)
      A(62)=B(62)
      A(65)=B(65)
      A(66)=B(66)

      A(67)=VDOT (A,B,7) + VDOT (A,B,0)
      A(68)=VDOT (A(17),B(17),7) + VDOT (A(17),B(17),0)

      A(69)=VDOTN (A,B,7) + VDOTN (A,B,0)
      A(70)=VDOTN (A(17),B(17),7) + VDOTN (A(17),B(17),0)

      A(71)=VDOTN2 (A,B,7) + VDOTN2 (A,B,0)
      A(72)=VDOTN2 (A(17),B(17),7) + VDOTN2 (A(17),B(17),0)

      CALL MVERIF (15,A(51),B(51),22)

      CALL NEWGUY ('VMATR-VMATL.','TVEC    ')
      K=5
      N=4

         DO 40 I=1,K
      B(I+500)=0.
      IA1=I

         DO 35 J=1,N
      B(I+500)=B(I+500) + B(J)*A(IA1)
   35 IA1=IA1+K
   40 CONTINUE

C--      A IS THE MATRIX,  B IS THE VECTOR
      CALL VMATR (B,A,A(501),N,K)
      CALL MVERII (21,A(501),B(501),K)

      IA1=1
         DO 50 I=1,K
      B(I+600)=0.

         DO 45 J=1,N
      B(I+600)=B(I+600) + B(J)*A(IA1)
   45 IA1=IA1+1
   50 CONTINUE

      CALL VMATL (A,B,A(601),K,N)
      CALL MVERII (22,A(601),B(601),K)

      RETURN
      END
