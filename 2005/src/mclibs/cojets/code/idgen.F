*
* $Id: idgen.F,v 1.1.1.1 1996/01/11 14:14:38 mclareni Exp $
*
* $Log: idgen.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:38  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE IDGEN
C     ****************
C          CALL SYSTEM DATE AND TIME ROUTINES (NON-STANDARD) TO SET UP
C          RUN IDENTIFICATION:
C          IDVER=100*VERSN     (INTEGER COJETS VERSION NUMBER)
C          IDG(1)=YYMMDD       (INTEGER YEAR-MONTH-DAY)
C          IDG(2)=HHMMSS       (INTEGER HOUR-MINUTE-SECOND)
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/idrun.inc"
#include "cojets/itapes.inc"
      CHARACTER*8 BF
#if defined(CERNLIB_IBM)||defined(CERNLIB_VAX)
      DIMENSION ND(2),NT(2)
#endif
      DIMENSION NDV(6),NTV(6)
C          DEFAULT RUN ID IS ZERO.
      IYMD=0.
      IHMS=0.
C          CALL DATIMH FOR DATE AND TIME. (IN CERN LIBRARY)
      CALL DATIMH(ND,NT)
C
      WRITE(BF,1) ND
#if defined(CERNLIB_CRAY)||defined(CERNLIB_CDC)
1     FORMAT(A8)
#endif
#if defined(CERNLIB_IBM)||defined(CERNLIB_VAX)
1     FORMAT(2A4)
#endif
      READ(BF,2) NDV
2     FORMAT(2I1,1X,2I1,1X,2I1)
      WRITE(BF,1) NT
      READ(BF,2) NTV
#if defined(CERNLIB_IBM)||defined(CERNLIB_VAX)
      IYMD=(10*NDV(1)+NDV(2))
     1    +(10*NDV(3)+NDV(4))*100
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_CDC)
      IYMD=(10*NDV(1)+NDV(2))*100
     1    +(10*NDV(3)+NDV(4))
#endif
     2    +(10*NDV(5)+NDV(6))*10000
      IHMS=(10*NTV(1)+NTV(2))*10000
     1    +(10*NTV(3)+NTV(4))*100
     2    +(10*NTV(5)+NTV(6))
      IDG(1)=IYMD
      IDG(2)=IHMS
      RETURN
      END
