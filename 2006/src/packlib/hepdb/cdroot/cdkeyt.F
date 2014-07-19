*
* $Id: cdkeyt.F,v 1.1.1.1 1996/02/28 16:24:28 mclareni Exp $
*
* $Log: cdkeyt.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:28  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDKEYT
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDKEYT                                                  *
*                                                                      *
*   Retrieves the I/O descriptor for the current directory             *
*                                                                      *
*   Called by various routines in the HEPDB package                    *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      DO 10 I = 1, NWKYCK
        IN     = (I - 1) / 10
        JB     = 3*I - 30*IN - 2
        IWD    = IQ(KOFSCD+LCDRCD+IKTYCD+IN)
        IFORM  = JBYT (IWD, JB, 3)
        IF (IFORM.EQ.3) THEN
          IFORM  = 5
        ELSE IF (IFORM.EQ.4) THEN
          IFORM  = 6
        ELSE IF (IFORM.EQ.5)  THEN
          IFORM  = 3
        ELSE IF (IFORM.LT.1.OR.IFORM.GT.5)  THEN
          IFORM  = 2
        ENDIF
        IOTYCK(I) = IFORM
   10 CONTINUE
*                                                             END CDKEYT
  999 END
