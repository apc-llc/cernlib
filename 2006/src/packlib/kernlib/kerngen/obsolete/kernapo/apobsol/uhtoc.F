*
* $Id: uhtoc.F,v 1.1.1.1 1996/02/15 17:51:10 mclareni Exp $
*
* $Log: uhtoc.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:10  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE UHTOC (MH,NPHP,MC,NCHP)
C
C CERN PROGLIB# M409    UHTOC           .VERSION KERNAPO  1.06  851211
C ORIG. 29/03/85  JZ
C

      DIMENSION    MH(99), NPHP(9), NCHP(9)
      INTEGER*2    MC(99)

      INTEGER*2    MCH2(2), ICOMP, ITOGGL
      EQUIVALENCE (MCH2,MCH4)


      NPH = NPHP(1)
      NCH = NCHP(1)
      IF   (NCH)             91,39,11
   11 NW     = RSHFT (NCH,1)
      JC     = 1
      ITOGGL = 0
      NPH    = MIN0 (NPH,4)
      NWH    = (NCH-1)/NPH + 1

C--                TAKE CHARACTERS FROM MH WORD BY WORD

      DO 29  JWH=1,NWH
      MWDH = MH(JWH)

      DO 28  JPH=1,NPH
      MCH4 = RSHFT(MWDH,24)
      MWDH = LSHFT(MWDH,8)
      IF (ITOGGL.NE.0)       GO TO 24
      ICOMP  = LSHFT(MCH2(2),8)
      ITOGGL = 1
      GO TO 27

   24 MC(JC) = OR (ICOMP,MCH2(2))
      JC     = JC + 1
      ITOGGL = 0
   27 NCH = NCH - 1
      IF (NCH.EQ.0)          GO TO 31
   28 CONTINUE
   29 CONTINUE


C--                PACK THE TRAILING WORD

   31 IF (ITOGGL.EQ.0)       RETURN
      MC(JC) = OR ( ICOMP, RSHFT (LSHFT(MC(JC),8), 8))
   39 RETURN

   91 CALL ABEND
      END
#ifdef CERNLIB_TCGEN_UHTOC
#undef CERNLIB_TCGEN_UHTOC
#endif
