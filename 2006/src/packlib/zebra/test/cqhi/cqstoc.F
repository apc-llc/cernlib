*
* $Id: cqstoc.F,v 1.1.1.1 1996/03/06 10:47:00 mclareni Exp $
*
* $Log: cqstoc.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQSTOC (NOCHK)

C-    Dump divisional occupation of the current store

#include "test_include/cqc.inc"
#include "zebra/mqsys.inc"
C--------------    End CDE                             --------------
      DIMENSION    IV(12)


      IF (NOCHK.LT.0)  IQCOPT(8)=-7

      CALL UCOPY (NQPNAM(KQT+1),NQSNAM,5)

      CALL CQHEAD (1,'.','CURRENT STORE PARAMETERS  -----.','HJIIIII'
     +,            'NAME,JQSTOR,/JQDIVI,/KQT,/KQS,/LQSTOR.')

      IV(1) = NQSNAM(1)
      IV(2) = NQSNAM(2)
      IV(3) = JQSTOR
      IV(4) = JQDIVI
      IV(5) = KQT
      IV(6) = KQS
      IV(7) = LQSTOR
      CALL CQDATA (IV(1),IV(7))

      CALL CQHEAD (0,  '.', '.', 'JJJJJJJ'
     +, 'NQFEND,NQSTRU,NQREF,NQLINK,*NQMINR,*LQ2END,JQDVLL,JQDVSY.')

      CALL CQDATA (NQFEND,JQDVSY)

      CALL CQHEAD (1,  '.', 'DIVISION OCCUPANCY.', 'JJOOOOH'
     +, 'JDIV,LQSTA,LQEND,NQDMAX,IQMODE,IQKIND,IQRTO,IQRNO,NAME.')

      JDV = 0
   21 JDV = JDV + 1
      IV(1) = JDV
      IV(2) = LQSTA(KQT+JDV)
      IV(3) = LQEND(KQT+JDV)
      IV(4) = NQDMAX(KQT+JDV)
      IV(5) = IQMODE(KQT+JDV)
      IV(6) = IQKIND(KQT+JDV)
      IV(7) = IQRTO(KQT+JDV)
      IV(8) = IQRNO(KQT+JDV)
      IV(9) = IQDN1(KQT+JDV)
      IV(10)= IQDN2(KQT+JDV)
      CALL CQDATA (IV(1),IV(10))

      IF (JDV.EQ.JQDVLL)  JDV=JQDVSY-1
      IF (JDV.LT.20)               GO TO 21
      IF (NOCHK.NE.0)  IQCOPT(8) = 0
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
