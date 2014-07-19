*
* $Id: detvtx.F,v 1.1.1.1 1996/03/08 16:58:51 mclareni Exp $
*
* $Log: detvtx.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:51  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE DETVTX(PRIVER)
C.----------------------------------------------------------------------
C.
C.    GENERATE PRIMARY INTERACTION VERTEX ACCORDING TO GAUSSIAN
C.    DISTRIBUTIONS IN X, Y AND Z. MAY BE MODIFIED BY THE USER!
C.    LAST UPDATE: 05/02/89
C.
C.----------------------------------------------------------------------
#include "eurodec/inivtx.inc"
      DIMENSION PRIVER(3)
C--
C--   LOOP OVER VERTEX COORDINATES
      DO 20 I=1,3
        JDUMMY=100*I
   10   U1=EURRAN(JDUMMY)
        U2=EURRAN(JDUMMY+1)
        V1=2.*U1-1.
        V2=2.*U2-1.
        SUM2=V1**2+V2**2
        IF (SUM2.GT.1.) THEN
          JDUMMY=JDUMMY+2
          GOTO 10
        ELSE
          Z1=V1*SQRT((-2.*LOG(SUM2))/SUM2)
C--
C--   INTRODUCE USER DEFINED WIDTHS (TITLES)
          PRIVER(I)=VTXSIG(I)*Z1
        ENDIF
   20 CONTINUE
      RETURN
      END
