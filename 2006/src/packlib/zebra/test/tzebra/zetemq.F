*
* $Id: zetemq.F,v 1.2 1996/04/18 16:14:42 mclareni Exp $
*
* $Log: zetemq.F,v $
* Revision 1.2  1996/04/18 16:14:42  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:05  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE ZETEMQ

#include "zebra/zbcd.inc"
#include "zebra/zstate.inc"
#include "zebra/zunit.inc"
#include "zebra/mzbits.inc"
#include "zebra/quest.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
#include "zebra/mzct.inc"
#include "test_include/cqc.inc"
#include "zebra/cqbkc.inc"
#include "test_include/cqbkceq.inc"
#include "test_include/testla.inc"
#include "test_include/testll.inc"
#include "test_include/testdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
C--------------    End CDE                             --------------
      CHARACTER    ID*2
      DIMENSION    MMFILL(5)
#if defined(CERNLIB_QMVDS)
      SAVE         MMFILL
#endif
      DATA  MMFILL / 4HFILL, 0, 0, 0, 0/

#include "zebra/q_jbyt.inc"

C     IQCOPT(2)= 7
C     NQDEVZ = 7

C----              Test Hollerith conversion first

      CALL ZETECE

C----              Initialize the test store

      ID  = '  '
      IF (JQSTOR.GE.0)  ID = '* '

      CALL CQHEAD (3,'MQ'//ID, 'INITIALIZE TEST STORE.','.','.')
      CALL CQSTOR (0)
      CALL CQSTOC (0)

C----              Lift the first structure

      CALL CQHEAD (2,'.', 'LIFT FIRST STRUCTURE.', '.','.')
      CALL MZSDIV (0,-1)
      CALL CQLODS (0,0)
      CALL UCOPY (LA1,LAREX,12)
      CALL UCOPY (LB4,LAREY,12)
      CALL CQSTOM (0)
      CALL CQDWSP  (0)

C----              Test MZFLAG

      CALL CQHEAD (2,'-', 'TEST MZFLAG.', 'PPPPPPP'
     +,                'A1,A2,A3,A4,A5,A6.')

      CALL MZSDIV (0,-1)
      CALL MZFLAG (IXSTOR,LQMAIN,1,'L')
      CALL MZFLAG (IXSTOR,LQMAIN,5,'LV')
      CALL MZFLAG (IXSTOR,LA3,   5,'Z')
      CALL MZFLAG (IXSTOR,LA3,   9,'V')
      CALL MZFLAG (IXSTOR,LD1,   2,'L')
      CALL MZFLAG (IXSTOR,LD1,   3,'V')
      CALL MZFLAG (IXSTOR,LL3,   2,'LV')

      DO 24  J=1,21
      L = LV(J)
   24 M(J) = JBYT (IQ(L),1,15)
      CALL CQDATA (M,M(6))

      CALL CQHEAD (0,'-','-','.', 'B1,B2,B3,B4,C1,C2,C3,C4,D1.')
      CALL CQDATA (M(7),M(15))

      CALL CQHEAD (0,'-','-','.', 'LOW1,LOW2,LOW3,LOW4,LOW5,LOW6.')
      CALL CQDATA (M(16),M(21))

      CALL CQHEAD (2,'.', 'TEST MZDROP.', '.','.')
      CALL MZSDIV (0,-1)
      CALL MZDROP (IXSTOR,LA5, '.')
      CALL MZDROP (IXSTOR,LB2, '.')
      CALL MZDROP (IXSTOR,LL4, 'L')
      CALL CQSTOM (0)
      CALL CQDWSP  (0)

C----              Test table building

      CALL CQHEAD (2,'.', 'TEST TABLE BUILDING, PART 1.', '.','.')
      MQDVGA = 3
      MQDVWI = 0
      JQSTMV = -1
      CALL MZTABM
      CALL CQMOTA (0, 0)

      CALL MZTABR
      CALL CQLRTA (0, 0)

      CALL CQHEAD (1,'.','EXQ CALL MZTABX/MZTABF -----------------------
     +------.', '.', '.')
      CALL MZTABX
      CALL MZTABF
      CALL CQMOTA (0, -1)
      CALL CQLRTA (0, -1)

      CALL CQHEAD (1,'.','EXQ CALL MZRELX/MZMOVE -----------------------
     +------.', '.', '.')
      CALL MZRELX
      CALL MZMOVE
      CALL CQSTOM (0)
      CALL CQDWSP  (0)

      CALL CQHEAD (2,'.', 'TEST TABLE BUILDING, PART 2.', '.','.')

      NGAP = 20
      CALL CQFIXU (2,NGAP)
      MMFILL(4) = NGAP - 12
      CALL MZLIFT (IXSTOR+20, LX1, 0, 2, MMFILL, 20)

      CALL SBIT1 (IQ(LX1),IQDROP)
      CALL SBIT1 (IQ(LL2),IQDROP)
      CALL SBIT1 (IQ(LA1),IQDROP)

      CALL CQSTOM (0)
      CALL CQDWSP  (0)

      CALL CQHEAD (1,'.','EXQ CALL TABLE ROUTINES  ---------------------
     +------.', '.', '.')
      MQDVGA = 3
      CALL SBIT1 (MQDVGA,20)
      MQDVWI = 0
      JQSTMV = JQSTOR
      JQDVM1 = 2
      JQDVM2 = 2
      CALL MZTABM
      CALL MZTABR
      CALL MZTABX
      CALL MZTABF
      CALL CQMOTA (0, 0)
      CALL CQLRTA (0, 0)

      CALL CQHEAD (1,'.','EXQ CALL MZRELX/MZMOVE -----------------------
     +------.', '.', '.')
      CALL MZRELX
      CALL MZMOVE
      CALL CQSTOM (0)
      CALL CQDWSP  (0)

C----              Lift extra divisions

      CALL CQHEAD (2,'.', 'CREATE DIVISIONS 3,20,19,4,5,18.', '.','.')
      CALL CQDIVI (3)
      CALL CQSTOC (0)

C----              Test ZSHUNT

      CALL CQHEAD (2,'.', 'TEST ZSHUNT.', '.','.')

      CALL SBIT1 (IQ(LA4),IQDROP)
      CALL SBIT1 (IQ(LB1),IQDROP)
      CALL ZSHUNT (IXSTOR,LQMAIN,LQUSER,1,1)
      CALL MZSDIV (0,-1)

      CALL CQLODS (3,0)
      CALL MZSDIV (0,-1)
      CALL ZSHUNT (IXSTOR,LQMAIN,LQUSER,-1,1)
      CALL MZSDIV (0,-1)
      CALL MZDROP (IXSTOR,LC1, 'L')
      CALL SBIT1 (IQ(LA2),IQDROP)

      CALL CQLODS (4,0)
      CALL MZDROP (IXSTOR,LQMAIN, 'L')

      CALL CQLODS (5,0)
      CALL ZSHUNT (IXSTOR,LQMAIN,LQUSER(2),1,1)
      CALL ZSHUNT (IXSTOR,LD1,LA5,-2,0)
      CALL MZDROP (IXSTOR,LB1, 'L')

      CALL CQLODS (18,0)
      CALL ZSHUNT (IXSTOR,LQMAIN,LQUSER(6),1,1)
      CALL MZDROP (IXSTOR,LA4, 'L')
      CALL SBIT1  (IQ(LA2),IQDROP)

      CALL CQLODS (19,0)
      CALL ZSHUNT (IXSTOR,LQMAIN,LQUSER(6),-2,1)

      CALL CQLODS (20,0)
      CALL ZSHUNT (IXSTOR,LQMAIN,LQUSER(7),2,1)
      CALL ZSHUNT (IXSTOR,LA2,LA5,0,0)
      CALL ZSHUNT (IXSTOR,LB1,LB4,0,0)
      CALL ZSHUNT (IXSTOR,LB2,LA4,-2,1)
      CALL ZSHUNT (IXSTOR,LC1,LB2,-2,1)
      CALL ZSHUNT (IXSTOR,LD1,LC3,-1,1)

      CALL CQSTOM (0)
      CALL CQDWSP  (0)

C----              Test MZIXCO + MZXREF

      CALL CQHEAD (2,'.', 'TEST MZIXCO + MZXREF.', 'OOOOOOO','.')

      CALL MZSDIV (0,-1)
      M(1) = MZIXCO (IXSTOR+1, IXSTOR+2,  0,0)
      M(2) = MZIXCO (IXSTOR+21,IXSTOR+22, 0,0)
      M(3) = MZIXCO (IXSTOR+1, IXSTOR+2, IXSTOR+3, IXSTOR+4)
      M(4) = MZIXCO (M(1),     IXSTOR+22, 0,0)
      M(5) = MZIXCO (M(1),     M(2),     IXSTOR+20, 0)
      M(6) = MZIXCO (M(4),     M(2),      0,0)
      CALL CQDATA (M,M(6))

      DO  44  J=1,6
   44 M(J) = JBYT (M(J),27,6)
      CALL CQDATA (M,M(6))

      IX1TO7 = MZIXCO (IXSTOR+1, IXSTOR+2, IXSTOR+3, IXSTOR+4)
      IX1TO7 = MZIXCO (IX1TO7,   IXSTOR+5, IXSTOR+6, IXSTOR+7)
      IX1114 = MZIXCO (IXSTOR+11,IXSTOR+12,IXSTOR+13,IXSTOR+14)
      IX6A12 = MZIXCO (IXSTOR+6, IXSTOR+12, 0,0)
      M(1) = JBYT (IX1TO7,27,6)
      M(3) = JBYT (IX1114,27,6)
      M(5) = JBYT (IX6A12,27,6)
      M(2) = IX1TO7
      M(4) = IX1114
      M(6) = IX6A12
      CALL CQDATA (M,M(6))

      CALL MZXREF (IXDV3, IX1TO7, 'A')
      CALL MZXREF (IXDV4, IX1TO7, 'A')
      CALL MZXREF (IXDV5, IX1TO7, '.')
      CALL MZSDIV (0,-1)

      CALL MZXREF (IXDV4, IXSTOR+9, 'A')
      CALL MZXREF (IXDV5, IXSTOR+9, 'A')

      CALL MZXREF (IXDV3, IX1114, 'A')
      CALL MZXREF (IXDV4, IX1114, 'A')

      CALL MZXREF (IXDV5, IXSTOR+22, 'A')
      CALL CQSTOC (0)

      CALL MZXREF (IXDV3, IXSTOR+13, 'R')
      CALL MZXREF (IXDV4, IX6A12, 'R')
      CALL MZXREF (IXDV5, IX6A12, 'R')
      CALL MZXREF (IXDV4, IXSTOR+21, 'R')
      CALL CQSTOC (0)

C----              Test MZPUSH

      CALL CQHEAD (2,'.', 'TEST MZPUSH.', '.','.')

      CALL CQSTOM (-7)
      CALL CQHEAD (1,'.','EXQ CALL MZPUSH WITHOUT RELOCATION  ----------
     +------.', '.', '.')

      L = LZFIDH (IXDV19, IDCV(5), 0)
      IF (L.NE.0)  CALL MZPUSH (IXDV19,L,0, 500, '.')
      IF (L.NE.0)  CALL MZPUSH (IXDV19,L,0,-300, 'R')

      L = LZFIDH (IXDV20, IDCV(5), 0)
      IF (L.NE.0)  CALL MZPUSH (IXDV20,L, 400,0, '.')
      IF (L.NE.0)  CALL MZPUSH (IXDV20,L,-360,0, 'R')

      L = LZFIDH (IXDV5,  IDAV(2), 0)
      IF (L.NE.0)  THEN
          ND = IQ(L-1)
          CALL VZERO (IQ(L+ND-50),50)
          CALL MZPUSH (IXDV5,L,0,-50, 'R')
        ENDIF

      L = LZFIDH (IXDV19, IDAV(5), 0)
      L = LZFIDH (IXDV19, IDAV(3), L)
      IF (L.NE.0)  CALL MZPUSH (IXSTOR,L, 140,200, 'I')
      IF (L.NE.0)  CALL MZPUSH (IXDV19,L,-100,  0, 'R')
      CALL CQSTOC (0)

      CALL CQHEAD (1,'.','EXQ CALL MZPUSH WITH RELOCATION  -------------
     +------.', '.', '.')

      L = LZFIDH (IXDV20, IDAV(4), 0)
      IF (L.NE.0)  CALL MZPUSH (IXDV20,L, 20,100, '.')

      CALL CQSTOC (0)
      CALL CQSTOM (0)

C----              Test Garbage collection

      CALL CQHEAD (2,'.', 'TEST GARBAGE COLLECTION WITHOUT MOVE.'
     +, '.','.')

      CALL CQSTOM (-7)
      CALL CQHEAD (1,'.','EXQ CALL MZGARB (DIV 22, 0)  -----------------
     +------.', '.', '.')
      CALL MZGARB (IXSTOR+22,0)
      CALL CQSTOC (0)
      CALL CQSTOM (0)
      CALL CQDWSP  (0)

      CALL CQHEAD (1,'.','EXQ CALL MZGARB (DIV 2, 18+19)  --------------
     +------.', '.', '.')
      IXDVWI = MZIXCO (IXSTOR+18,IXSTOR+19,0,0)
      CALL MZGARB (IXSTOR+2,IXDVWI)
      CALL CQSTOC (0)
      CALL CQSTOM (0)
      CALL CQDWSP  (0)

      CALL CQHEAD (2,'.', 'TEST GARBAGE COLLECTION WITH MOVE.'
     +, '.','.')

      NGAP = 20
      CALL CQFIXU (2,NGAP)
      MMFILL(4) = NGAP + 171
      CALL MZLIFT (IXSTOR+20, LX1, 0, 2, MMFILL, 0)
      CALL SBIT1 (IQ(LX1),IQDROP)

      CALL CQSTOC (0)
      CALL CQSTOM (0)
      CALL CQDWSP (0)
      CALL MZEND

C----              Test inquiries

      CALL MZLOGL (IXSTOR,2)
      CALL MZINQS (IXSTOR)
      JDVLL = IQUEST(9)
      JDVSY = IQUEST(10)

      JDVA = 1
      JDVE = JDVLL
   73 DO 74  JDV=JDVA,JDVE
      CALL MZINQD (IXSTOR+JDV)
   74 CONTINUE
      IF (JDVA.EQ.1)  THEN
          JDVA = JDVSY
          JDVE = 20
          GO TO 73
        ENDIF
      CALL MZLOGL (IXSTOR,2)

C----              Test MZREPL

      CALL ZETERE (7)

      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
