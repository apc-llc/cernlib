*
* $Id: fqbkco.F,v 1.1.1.1 1996/03/06 10:47:01 mclareni Exp $
*
* $Log: fqbkco.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:01  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE FQBKCO

C-    Read back with fan-out through FZCOPY

#include "zebra/zstate.inc"
#include "zebra/zunit.inc"
#include "zebra/mzbits.inc"
#include "zebra/quest.inc"
C! +CDE, FZCI, FZCSEG.
#include "zebra/fzcseg.inc"
#include "test_include/testll.inc"
#include "test_include/testdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
#include "test_include/fqtlun.inc"
C--------------    END CDE                             --------------
      DIMENSION    MHEA(10,3)


C-------           Copy types 902 + 903

      CALL ZPHASE (3)
      LUNIN = LUNT1


   24 MHEA(1,1) = 9
      CALL FZIN (LUNIN,IXSTOR,0,0,'S',MHEA(1,1),MHEA(2,1))
      IF (IQUEST(1))              24, 26, 29
   26 IF (MHEA(2,1).EQ.901)        GO TO 24
      LUNFZ = LUNT2
      IF (MHEA(2,1).EQ.903)  LUNFZ=LUNT3

      CALL FZCOPY (LUNIN, LUNFZ, 0, '.', 2,MHEA(1,1),MHEA(2,1))
      IF (IQUEST(1).NE.0)          GO TO 94
      GO TO 24

   29 IF (IQUEST(1).LT.5)          GO TO 24

      CALL FZENDI (LUNT1,'I')
      CALL FZENDO (LUNT2,'I')
      CALL FZENDO (LUNT3,'I')

C-------           Read back and compare

      CALL FQBKFI
      RETURN

   94 CALL ZFATAM ('FQBKCO - unexpected exit')
      END
*      ==================================================
#include "zebra/qcardl.inc"
