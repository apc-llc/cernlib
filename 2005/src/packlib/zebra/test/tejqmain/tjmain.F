*
* $Id: tjmain.F,v 1.1.1.1 1996/03/06 10:47:05 mclareni Exp $
*
* $Log: tjmain.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:05  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      PROGRAM TJMAIN

#include "zebra/zunit.inc"
#undef CERNLIB_ZEBRA_ZUNIT_INC
#include "test_include/testll.inc"
#include "test_include/mainiq.inc"
C--------------    End CDE                             --------------
#include "test_include/tpseudo.inc"
* Ignoring t=pass
      DIMENSION    LISTIN(4)


      DATA  LISTIN / 3, 0,-1, 0 /

      OPEN (16,FILE='jztest.dat',STATUS='OLD')

      CALL MZEBRA (LISTIN)
      CALL MZVERS
      CALL CQBKIN

      WRITE (IQLOG,9011)
 9011 FORMAT (1X/' *!!!!!!  Test JZ91 package')

      CALL ZVAUTO (0, 0,-1, 0)
      CALL JQTEST

      WRITE (IQLOG,9079)
 9079 FORMAT (1X/' *!!!!--  End of test JZ91')

      CALL ZEND
      END

      SUBROUTINE ZVAUTX
      CALL ZVAUTY
      END
*      ==================================================
#include "zebra/qcardl.inc"
