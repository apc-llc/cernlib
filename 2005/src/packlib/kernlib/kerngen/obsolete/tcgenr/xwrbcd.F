*
* $Id: xwrbcd.F,v 1.1.1.1 1996/02/15 17:53:28 mclareni Exp $
*
* $Log: xwrbcd.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:28  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XWRBCD (ITAPE,M,NW,NCH)
C
C
C--               ROUTINE COMMON TO THE BASIC AND THE EXTENDED XPACKAGE.
C
      DIMENSION M(100)
      DATA  NCHW/10/                                                    A10
C     DATA  NCHW/ 8/                                                    A8
C     DATA  NCHW/6/                                                     A6
C     DATA  NCHW/5/                                                     A5
C     DATA  NCHW/4/                                                     A4
C
      NWS= NW
      IF (NWS.LE.0)  NWS=(NCH-1)/NCHW + 1
      CALL XOT123 (ITAPE,0,M,NWS)
      RETURN
      END
