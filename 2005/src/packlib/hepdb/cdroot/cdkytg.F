*
* $Id: cdkytg.F,v 1.1.1.1 1996/02/28 16:24:29 mclareni Exp $
*
* $Log: cdkytg.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:29  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDKYTG
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDKYTG                                                  *
*                                                                      *
*   Retrieves the I/O descriptor and the Tags of the Key elements      *
*   of the current directory                                           *
*                                                                      *
*   Called by CDDONT, CDENFZ, CDENTB, CDKOUT, CDPART, CDPRIN           *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
      DIMENSION       IHTAG(2)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      KTAG   = IKTYCD + (NWKYCK-1)/10 + 1
      DO 10 I = 1, NWKYCK
        CALL ZITOH (IQ(KOFSCD+LCDRCD+KTAG+2*I-2), IHTAG, 2)
        CALL UHTOC (IHTAG, 4, CTAGCK(I), 8)
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
*                                                             END CDKYTG
  999 END
