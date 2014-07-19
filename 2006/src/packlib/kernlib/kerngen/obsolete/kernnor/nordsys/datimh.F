*
* $Id: datimh.F,v 1.1.1.1 1996/02/15 17:54:51 mclareni Exp $
*
* $Log: datimh.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:51  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      SUBROUTINE DATIMH (ND,NT)
C
C CERN PROGLIB# Z007    DATIMH          .VERSION KERNNOR  0.99  771101
C
      DIMENSION ND(2),NT(2),CH(8),IH(7),CA(10)
      DATA CA/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9/
      DATA SLA/1H//,DOT/1H./
 
      CALL CLOCK (IH)
      CH(1) = CA(1+IH(5)/10)
      CH(2) = CA(1+MOD(IH(5),10))
      CH(3) = SLA
      CH(4) = CA(1+IH(6)/10)
      CH(5) = CA(1+MOD(IH(6),10))
      CH(6) = SLA
      CH(7) = CA(1+MOD(IH(7)/10,10))
      CH(8) = CA(1+MOD(IH(7),10))
      CALL UBUNCH (CH,ND,8)
      CH(1) = CA(1+IH(4)/10)
      CH(2) = CA(1+MOD(IH(4),10))
      CH(3) = DOT
      CH(4) = CA(1+IH(3)/10)
      CH(5) = CA(1+MOD(IH(3),10))
      CH(6) = DOT
      CH(7) = CA(1+IH(2)/10)
      CH(8) = CA(1+MOD(IH(2),10))
      CALL UBUNCH (CH,NT,8)
      RETURN
      END
#ifdef CERNLIB_TCGEN_DATIMH
#undef CERNLIB_TCGEN_DATIMH
#endif
