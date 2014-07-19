*
* $Id: mztabs.F,v 1.3 1999/06/18 13:30:32 couet Exp $
*
* $Log: mztabs.F,v $
* Revision 1.3  1999/06/18 13:30:32  couet
* - qcardl.inc was empty: It is now removed and not used.
*   Comment END CDE removed.
*
* Revision 1.2  1996/04/18 16:12:49  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:20  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      SUBROUTINE MZTABS

C-    Update Memory Occupation table and MQDVAC for shifting
C-    divisions JQDVM1 to JQDVM2 by NQDVMV words

#include "zebra/mqsys.inc"
#include "zebra/mzct.inc"
*

#include "zebra/q_sbit1.inc"

      LMT  = LQMTA
   21 LMT  = LMT + 8
      JDIV = LQ(LMT)
      IF (JDIV.LT.JQDVM1)          GO TO 21
      IF (JDIV.GT.JQDVM2)            RETURN

      LQ(LMT+2) = LQ(LMT+2) + NQDVMV

      IF (LQ(LMT+1).LT.0)          GO TO 21
      IF (LQ(LMT+1).GE.2)          GO TO 21
      LQ(LMT+1) = 2
      MQDVAC = MSBIT1 (MQDVAC,JDIV)
      GO TO 21
      END
