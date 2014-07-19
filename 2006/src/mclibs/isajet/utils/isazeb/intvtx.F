*
* $Id: intvtx.F,v 1.10 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: intvtx.F,v $
* Revision 1.10  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE INTVTX(XYZ)
C-------------------------------------------------------
C-
C-  Subroutine to generate an interaction vertex.
C-
C-  Default is to generate an interaction point with normal distribution
C-  along z given by SIGZ= 25 cm and with x=0,y=0.
C-
C-  If SETVTX is called the interaction point returned in the next
C-  call of INTVTX will be the values given to SETVTX.
C-  SETVTX must be called for every event before event generation
C-  to overwrite the default.
C-
C-  OUTPUT:
C-  XYZ = x,y,z position of vertex
C-
C-  ENTRY SETVTX(XYZ):
C-  INPUT:
C-  XYZ=  x,y,z position of vertex
C-
C-    SDP Nov.,1985
C-
C-------------------------------------------------------
C
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      REAL XYZ(3)
      REAL A,B,ZSIG,XSET(3)
      LOGICAL SETXYZ
      DATA SETXYZ/.FALSE./
      DATA ZSIG/0.0/   ! sigma of interaction region
C
      IF(SETXYZ) THEN
       CALL UCOPY(XSET,XYZ,3)
      ELSE
        XYZ(1)=0
        XYZ(2)=0
        CALL RANNOR(A,B)
        XYZ(3)=ZSIG*A
      ENDIF
      SETXYZ=.FALSE.
C
      RETURN
      ENTRY SETVTX(XYZ)
C
C      A call to SETVTX forces vertex to have values XYZ
C
      SETXYZ=.TRUE.
      CALL UCOPY(XYZ,XSET,3)
      RETURN
      END
