*
* $Id: datime.F,v 1.1.1.1 1996/02/15 17:54:27 mclareni Exp $
*
* $Log: datime.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:27  mclareni
* Kernlib
*
*
#include "kerndge/pilot.h"
      SUBROUTINE DATIME (ND,NT)
C
C CERN PROGLIB# Z007    DATIME          .VERSION KERNDGE  1.07  891003
C ORIG. 03/02/86  JZ
C
C-    CALL DATIME (ND,NT)   RETURNS INTEGER DATE   ND = YYMMDD
C-                                  INTEGER TIME   NT =   HHMM
C
      COMMON /SLATE/ MM(40)
 
 
      CALL DATE (MM)
      CALL TIME (MM(4))
 
      MM(9) = MOD (MM(1),100)
 
      ND = (MM(9)*100+MM(2))*100 + MM(3)
      NT =            MM(4) *100 + MM(5)
      RETURN
      END
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
