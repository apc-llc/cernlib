*
* $Id: zvertx.F,v 1.11 2001/10/08 14:03:18 mclareni Exp $
*
* $Log: zvertx.F,v $
* Revision 1.11  2001/10/08 14:03:18  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ZVERTX(ZV,DZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy routine returns Z and DZ of vertx
C-
C-   Inputs  : None
C-   Outputs : ZV,DZ   Z position and Spread of Vertex
C-   Controls: None
C-
C-   Created  23-MAR-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "zebcom.inc"
#include "izisv1.inc"
      INTEGER LISV1
      REAL ZV,DZ
C----------------------------------------------------------------------
      ZV = 0.0
      DZ = 0.0
C
      IF(LISAE.NE.0) THEN
        LISV1=LQ(LISAE-IZISV1)
        IF(LISV1.NE.0) ZV=Q(LISV1+9)
      ENDIF
C
  999 RETURN
      END
