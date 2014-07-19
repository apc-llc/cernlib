*
* $Id: fqunus.F,v 1.1.1.1 1996/03/06 10:47:02 mclareni Exp $
*
* $Log: fqunus.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE FQUNUS

C-    Test closing of unused files

#include "zebra/zunit.inc"
#include "test_include/fqtlun.inc"
C--------------    END CDE                            ---------------

      WRITE (IQLOG,9001)
 9001 FORMAT (1X/' FQUNUS.  ------ Test closing of unused files ----')

      CALL FZFILE (LUNT2, 0, 'IO')
      CALL FZFILE (LUNT1, 0, 'IO')

      CALL FZENDI (LUNT2,'T')
      CALL FZENDO (LUNT1,'T')

      CALL FZFILE (LUNT2, 0, 'IO')
      CALL FZENDI (LUNT2,'T')

      WRITE (IQLOG,9004)
 9004 FORMAT (1X/' FQUNUS.  ----------------   End of test'/)

      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
