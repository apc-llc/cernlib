*
* $Id: cqlods.F,v 1.1.1.1 1996/03/06 10:47:00 mclareni Exp $
*
* $Log: cqlods.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQLODS (JDIV,JFILL)

C-    Create standard low level test structure in division JDIV

#include "zebra/zbcd.inc"
#include "zebra/cqbkc.inc"
#include "test_include/cqbkceq.inc"
#include "test_include/testla.inc"
#include "test_include/testll.inc"
#include "test_include/testdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
C--------------    End CDE                             --------------

      DIMENSION    FIXNUM(10)
#if !defined(CERNLIB_QMVDS)
      DOUBLE PRECISION  DPW(5)
#endif
#if defined(CERNLIB_QMVDS)
      DIMENSION         DPW(10)
      SAVE         FIXNUM
#endif
      DATA         FIXNUM /1., -1., 1.1E4, -1.1E4, 1.1E-4, -1.1E-4
     +,                    1.1E20, -1.1E20, 1.1E-20, -1.1E-20 /

C--                Lift chain  A1 - A6

      MMBANK(4) = 46
      MMBANK(3) =  4
      MMBANK(2) =  9
      MMBANK(1) = IDAV(6)
      CALL MZLIFT (IXSTOR+JDIV, LA6, LQMAIN,1, MMBANK, 0)

      DO 22  J=1,4
      JW = 6-J
      MMBANK(4) = MMBANK(4) + 10
      MMBANK(1) = IDAV(JW)
      CALL MZLIFT (IXSTOR, LAV(JW), LQMAIN,1, MMBANK, 0)
   22 CONTINUE

      MMBANK(1) = IDAV(1)
      CALL MZLIFT (IXSTOR+JDIV, LA1, LA2,1, MMBANK, 0)

C--                Lift chain  B1 - B4  at link -1 of A3

      MMBANK(4) = 46

      DO 24  J=1,4
      JW = 5-J
      MMBANK(1) = IDBV(JW)
      CALL MZLIFT (IXSTOR, LBV(JW), LA3, -1, MMBANK, 0)
   24 CONTINUE

C--                Lift chain  C1 - C4 at link -2 of B2

      MMBANK(4) = MMBANK(4) + 10
      MMBANK(3) = 1
      MMBANK(1) = IDCV(1)
      CALL MZLIFT (IXSTOR,LC4, LB2,-2, MMBANK, 0)

      DO 26  J=2,4
      JW = 5-J
      MMBANK(1) = IDCV(J)
      CALL MZLIFT (IXSTOR+JDIV, LCV(JW), LCV(JW+1),0, MMBANK, 0)
   26 CONTINUE
      L = LC4
      CALL ZTOPSY (IXSTOR,L)
      LQ(LC3-2) = LA1

C--                Lift bank D1 (NS=0) at link -1 of C4

      CALL MZLIFT (IXSTOR,LD1,LC4,-1,MMBKD,0)
      NBANKS = 15

C--                Lift chain  LOW1 - LOW6  at link -2 of A3

      IF (JDIV.NE.0)               GO TO 31
      NBANKS = 21
      MMBANK(2) = MMBANK(2) + 10
      MMBANK(3) =  4
      MMBANK(4) = 46

      MMBANK(1) = IDLV(1)
      CALL MZLIFT (IXSTOR+1, LL1, LA3, -2, MMBANK, 0)

      DO 28  J=2,6
      MMBANK(4) = MMBANK(4) + 10
      MMBANK(1) = IDLV(J)
      CALL MZLIFT (IXSTOR+1, LLV(J), LLV(J-1), 0, MMBANK, 0)
   28 CONTINUE

C----              Fill data parts

   31 DO 49  JJ=1,NBANKS
      L  = LV(JJ)
      ND = IQ(L-1)
      IQ(L-5)  = JJ
      IQ(L+1)  = 2
      IQ(L+2)  = IDVV(JJ)
      IQ(L+3)  = IQLETT(JJ)
      IQ(L+4)  = MIN (ND,15) - 4
      IQ(L+5)  = JJ

C--       Dynamic sector  *I, words (4), 5-15

      IF (JFILL.LT.0)              GO TO 49
      N  = MIN (ND,15)
      IF (N.LT.6)                  GO TO 49
      DO 34  J=6,N
   34 IQ(L+J) = JJ + J - 21

C--       Static sector  35F, words 16-50, fill floating integers

      IF (JFILL.EQ.0)              GO TO 49
      N  = MIN (ND,40)
      IF (N.LT.16)                 GO TO 49
      DO 36  J=16,N
   36 Q(L+J) = FLOAT (JJ + J - 31)

C--       Indefinite sector  -F, filling words 61-70, fractional fl.

      IF (JFILL.EQ.1)              GO TO 49
      N  = MIN (ND,70)
      IF (N.LT.61)                 GO TO 49
      DO 37  J=61,N
   37 Q(L+J) = FIXNUM(J-60) * Q(L+J-40)

   49 CONTINUE

C--       4 Double prec. numbers in words 51-58 of bank D1 only

      IF (JFILL.LT.2)              GO TO 55
      IF (IQ(LD1-1).LT.60)         GO TO 55
      CALL VZERO (DPW,10)

#if !defined(CERNLIB_QMVDS)
      DO 53  J=1,4
      DPW(J) = 1.
      DPW(J) = DPW(J) + 2.**(28+J)
   53 DPW(J) = DPW(J) + 2.**(43+J)
      DPW(5) = 8448.
#endif
#if defined(CERNLIB_QMVDS)
      DO 53  J=1,8,2
      DPW(J) = 1.
   53 DPW(J) = DPW(J) + 2.**(20+J)
      DPW(9) = 8448.
#endif
      CALL UCOPY (DPW,IQ(LD1+51),10)
   55 CONTINUE

C--                Copy links to /LAREZ/

      CALL UCOPY (LA1,LAREZ,12)
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
