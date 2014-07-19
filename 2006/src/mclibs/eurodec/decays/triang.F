*
* $Id: triang.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: triang.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION TRIANG(A,B,C)
C.----------------------------------------------------------------------
C.
C.    TRIANG = SQRT(LAMBDA(A**2,B**2,C**2))/(2*A)
C.    LAST UPDATE: 10/04/88
C.
C.----------------------------------------------------------------------
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION DA,DB,DC,DAPB,DAMB,DTRIAN
#endif
      DA=A
      DB=B
      DC=C
      DAPB=DA+DB
      DAMB=DA-DB
      DTRIAN=(DAMB-DC)*(DAPB+DC)*(DAMB+DC)*(DAPB-DC)
      TRIANG=DTRIAN
      TRIANG=SQRT(TRIANG)/(A+A)
      RETURN
      END
