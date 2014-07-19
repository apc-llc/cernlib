*
* $Id: zetere.F,v 1.3 1996/04/18 16:14:44 mclareni Exp $
*
* $Log: zetere.F,v $
* Revision 1.3  1996/04/18 16:14:44  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.2  1996/04/02 22:46:11  thakulin
* Support for EPC Fortran:  remove char-int equivalences and use F90
* transfer facility.
*
* Revision 1.1.1.1  1996/03/06 10:47:05  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE ZETERE (INLINE)

C-    Test MZREPL
C-        INLINE = zero for stand-alone test
C-                 non-zero if called in-line from ZETEMQ

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

      EQUIVALENCE (LSUP,LQUSER(1))
      EQUIVALENCE (LIXO,LQUSER(3)), (LIX,LX1)
      EQUIVALENCE (LOLD,LX2),       (LNEW,LX3)

      CHARACTER    CHID*4, CHWK*4
#if defined(CERNLIB_F90) || defined(CERNLIB_QFEPC)
      EQUIVALENCE (NNL,MMBANK(2))
#else
      EQUIVALENCE (CHID,MMBANK(1)), (NNL,MMBANK(2))
#endif
     +,            (NNS,MMBANK(3)), (NND,MMBANK(4))

      DIMENSION    MMSUP(5), MMIX(5)
      DATA  MMSUP  / 4HSUP , 2, 1, 0, 0 /
      DATA  MMIX   / 4HIX  , 2, 2, 1, 1 /


C----              Initialize the test store if stand-alone

      IF (INLINE.EQ.0)  CALL CQSTOR (0)

C----              Re-init the store and lift the test structure

      ID    = '  '
      IF (NQSTOR.GT.0)  ID = '* '

      CALL CQHEAD (3,'MR'//ID, 'TEST MZREPL WITH KEEP.', '.','.')
      CALL CQSTOR (-1)

C!    IQCOPT(2)= 7
C!    CALL MZLOGL (IXSTOR,2)
C!    CALL MZLOGL (IXSTOR,103)
C----              Loop with ITER =0 and =1 for KEEP and DROP

      ITER = 0

C--       Lift support bank, such that there is an 'up' link

   42 CALL MZLIFT (IXSTOR, LSUP, LSUP,1, MMSUP, 0)
      IXIX  = IXSTOR + 1

C--       Create the test structure

      CALL MZSDIV (0,-1)
      IF (ITER.EQ.0)  THEN
          CALL CQLODS (0,0)
          IXOLD = IXSTOR + 2
        ELSE
          CALL CQLODS (3,0)
          IXOLD = IXSTOR + 3
        ENDIF
      CALL ZSHUNT (IXSTOR,LQMAIN,LSUP,-1, 1)
      LQ(LSUP-2) = LA3
      LQ(LC1-2)  = LA1
      LQ(LC2-2)  = LA2
      LQ(LC3-2)  = LA3
      LQ(LC4-2)  = LA4

      CALL UCOPY (LA1,LAREX,12)
      CALL UCOPY (LB4,LAREY,12)
      CALL CQSTOM (-7)
      CALL CQDWSP (-7)

C--       Create the new banks and the index banks

      LOLD = LA2
      LIXO = 0
      DO 44  JDO=1,4

      CALL MZLIFT (IXIX,LIX, LIXO,1, MMIX, 0)
      LQ(LIX-1) = LOLD

      MMBANK(1) = IQ(LOLD-4)
      MMBANK(2) = IQ(LOLD-3)
      MMBANK(3) = IQ(LOLD-2)
      MMBANK(4) = IQ(LOLD-1)
#if defined(CERNLIB_F90) || defined(CERNLIB_QFEPC)
      CHID = transfer(MMBANK(1),'abcd');
#endif
      CHWK = 'N' // CHID(1:3)
      CHID = CHWK
#if defined(CERNLIB_F90) || defined(CERNLIB_QFEPC)
      MMBANK(1) = transfer(CHID,MMBANK(1));
#endif
      CALL MZLIFT (IXOLD,LNEW, LIX,-2, MMBANK,-1)
      IF (NNL.GT.0)  CALL UCOPY (LQ(LOLD-NNL),LQ(LNEW-NNL),NNL)
      IF (NND.GT.0)  CALL UCOPY (IQ(LOLD+1),  IQ(LNEW+1),  NND)
      LOLD = LQ(LOLD)
   44 CONTINUE

C--       Execute replacement

      IF (ITER.EQ.0)  THEN
          CALL MZREPL (IXSTOR, LIXO, 'K')
        ELSEIF (ITER.EQ.1)  THEN
          CALL MZREPL (IXSTOR, LIXO, '.')
          CALL MZGARB (IXSTOR+21,0)
        ELSE
          CALL MZREPL (IXSTOR, LIXO, 'I')
          CALL MZGARB (IXSTOR+21,0)
        ENDIF

      CALL CQSTOM (0)
      CALL CQDWSP (0)

      IF (ITER.GE.2)               RETURN
      ITER = ITER + 1

C--       2nd + 3rd iteration with ITER = 1 + 2

      IF (ITER.EQ.1)  THEN
          CALL CQHEAD (2,'.', 'TEST MZREPL WITH DROP.', '.','.')
          CALL CQDIVI (1)
        ELSE
          CALL CQHEAD (2,'.', 'TEST MZREPL WITH I.', '.','.')
        ENDIF
      CALL MZWIPE (IXSTOR+21)
      GO TO 42
      END
*      ==================================================
#include "zebra/qcardl.inc"
