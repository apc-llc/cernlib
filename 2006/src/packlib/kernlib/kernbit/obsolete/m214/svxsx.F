*
* $Id: svxsx.F,v 1.1.1.1 1996/02/15 17:47:36 mclareni Exp $
*
* $Log: svxsx.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:36  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
#if defined(CERNLIB_QMIBMVF)
@PROCESS DIRECTIVE ('*VDIR:') VECTOR OPT(3)
#endif
      SUBROUTINE SVXSX(IARRAY,NWORDS)
C
C             This is a subroutine to convert from VAX floating
C             point format (32 Bits) to IBM floating point format
C             (32 bits).
C
C          IARRAY   an area of storage 32*NWORDS bits long in which
C                   are stored NWORDS 32 bits VAX floating point
C                   numbers.
C                    On output it will contain NWORDS 32 bits IBM
C                    floating point numbers
C
C           NWORDS   number of floating point numbers to convert
C
C     VAX format : mmmm mmmm mmmm mmmm seee eeee emmm mmmm
C     IBM format : seee eeee mmmm mmmm mmmm mmmm mmmm mmmm
C
C
C Author: M.Roethlisberger/IBM         Optimize/Vectorize
C Date:   29-01-90
C
C Original Code written by Federico Carminati
C
C ----------------------------------------------------------------------
      DIMENSION J1(0:3), J2(0:3), J3(0:3)
      DIMENSION IARRAY(*)
      PARAMETER (LVMIN  =         25 )
C     PARAMETER (MA0180 = Z 00000180 )
      PARAMETER (MA0180 =        384 )
C     PARAMETER (MA7E00 = Z 00007E00 )
      PARAMETER (MA7E00 =      32256 )
C     PARAMETER (MA8000 = Z 00008000 )
      PARAMETER (MA8000 =   00032768 )
C     PARAMETER (MSKB23 = Z 00800000 )
      PARAMETER (MSKB23 =   08388608 )
C
C
C For Info:   32 = Z 20,   33 = Z 21
C             Z 20000000   Z 21000000   Z 21000000   Z 21000000
      DATA J1 /536870912,   553648128,   553648128,   553648128/
      DATA J2 /        1,           8,           4,           2/
      DATA J3 /        0,           4,           2,           1/

      IF (NWORDS.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR
        DO 1 J=1,NWORDS
          JLFT      = ISHFT (IAND(IARRAY(J),MA0180),-7)
C
C Take the 6 first bits of VAX exp, shift them 15 positions to
C the left and add 33 or 32, 24 positions shifted to the left.
C ------------------------------------------------------------
          JEXP      = ISHFT (IAND (IARRAY(J),MA7E00),15)+ J1(JLFT)
          JSGN      = ISHFT (IAND (IARRAY(J),MA8000),16)
          JMANT     = (IOR (IOR(ISHFT(ISHFT(IARRAY(J),25),-9),ISHFT
     .                (IARRAY(J),-16)),MSKB23) + J3(JLFT))/J2(JLFT)
C
C Make sure not exact 0
C
          JARRAY    = IARRAY (J)
          IF (JARRAY.NE.0) IARRAY(J) = IOR(IOR(JSGN,JEXP),JMANT)
   1    CONTINUE

      ELSE IF (NWORDS.GT.0) THEN
C*VDIR: PREFER SCALAR
        DO 2 J=1,NWORDS
          JLFT      = ISHFT (IAND(IARRAY(J),MA0180),-7)
          JEXP      = ISHFT (IAND (IARRAY(J),MA7E00),15)+ J1(JLFT)
          JSGN      = ISHFT (IAND (IARRAY(J),MA8000),16)
          JMANT     = (IOR (IOR(ISHFT(ISHFT(IARRAY(J),25),-9),ISHFT
     .                (IARRAY(J),-16)),MSKB23) + J3(JLFT))/J2(JLFT)
          JARRAY    = IARRAY (J)
          IF (JARRAY.NE.0) IARRAY(J) = IOR(IOR(JSGN,JEXP),JMANT)
   2    CONTINUE
      ENDIF

      END
