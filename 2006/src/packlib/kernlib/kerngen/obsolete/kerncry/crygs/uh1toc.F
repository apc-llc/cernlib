*
* $Id: uh1toc.F,v 1.1.1.1 1996/02/15 17:52:40 mclareni Exp $
*
* $Log: uh1toc.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:40  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE UH1TOC (MVH,VC,NCH)
C
C CERN PROGLIB# M409    UH1TOC          .VERSION KERNCRY  1.06  870527
C ORIG. 20/05/87  Federico Carminati + Julius Zoll
C
      CHARACTER*(*) VC
      DIMENSION    MVH(9)

      IF (NCH.LE.0)          RETURN

      DO 16  JCH=1,NCH
      ICH = SHIFT(MVH(JCH),8) .AND. MASK(128-8)
   16 VC(JCH:JCH) = CHAR(ICH)
      RETURN
      END
#ifdef CERNLIB_TCGEN_UH1TOC
#undef CERNLIB_TCGEN_UH1TOC
#endif
