*
* $Id: cqdtab.F,v 1.1.1.1 1996/03/06 10:47:00 mclareni Exp $
*
* $Log: cqdtab.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQDTAB (IFLAG)

C-    Print memory tables for debug printing only



      CALL CQMOTA (-7, IFLAG)
      CALL CQLRTA (-7, IFLAG)
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
