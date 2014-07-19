*
* $Id: sxsvx.F,v 1.1.1.1 1996/02/15 17:47:36 mclareni Exp $
*
* $Log: sxsvx.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:36  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
#if defined(CERNLIB_QMIBMVF)
@PROCESS DIRECTIVE ('*VDIR:')
#endif
      SUBROUTINE SXSVX(IARRAY,NWORDS)
C
C             This is a subroutine to convert from IBM short floating
C             point format (32 bits) to VAX short floating point format
C             (32 Bits)
C
C             IARRAY   an area of storage 32*NWORDS bits long in which
C                      are stored NWORDS 32 bits IBM floating point
C                      numbers. On output it will contain NWORDS
C                      32 bits VAX floating point numbers
C
C             NWORDS   number of floating point numbers to convert
C
C
C     IBM format : seee eeee mmmm mmmm mmmm mmmm mmmm mmmm
C     VAX format : mmmm mmmm mmmm mmmm seee eeee emmm mmmm
C
C
      DIMENSION IARRAY(*)
      DIMENSION J1(0:15), J2(0:15)

      DATA MSKA24 /Z 00FF FFFF/
      DATA MSKB24 /Z 00F0 0000/
      DATA MSKB31 /Z 7F00 0000/
      DATA MSKA32 /Z 8000 0000/
      DATA IBIG   /Z FFFF 7FFF/
      DATA ISMA   /Z 0000 0080/
      DATA J1     /    0,  384,  256,  256,  128,  128,  128,  128,
     .                 0,    0,    0,    0,    0,    0,    0,    0/
      DATA J2     /    1,    8,    4,    4,    2,    2,    2,    2,
     .                 1,    1,    1,    1,    1,    1,    1,    1/


      PARAMETER  (LVMIN  =  20)

      IF (NWORDS.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR
        DO 1 J=1,NWORDS
          INDX   = ISHFT (IAND (MSKB24,IARRAY(J)),-20)
          JEXP   = ISHFT (IAND (MSKB31,IARRAY(J)),-15)-J1(INDX)-16384
          JSGN   = ISHFT (IAND (MSKA32,IARRAY(J)),-16)
          JMANT  = IAND  (MSKA24,IARRAY(J))*J2(INDX)
          JDUM   = IOR   (ISHFT(JMANT,16),ISHFT(ISHFT(JMANT,9),-25))
          JDUM   = IOR   (IOR (JDUM,JEXP),JSGN)
C
C 127   is   0   7 positions left-shifted.
C 32649 is 255   7 positions left-shifted.
C
          IF (JEXP.LT.   127) JDUM = ISMA
          IF (JEXP.GT. 32640) JDUM = IBIG

          IF (IARRAY(J).NE.0) IARRAY(J) = JDUM
   1    CONTINUE

      ELSE IF (NWORDS.GT.0) THEN
C*VDIR: PREFER SCALAR
        DO 2 J=1,NWORDS
          INDX   = ISHFT (IAND (MSKB24,IARRAY(J)),-20)
          JEXP   = ISHFT (IAND (MSKB31,IARRAY(J)),-15)-J1(INDX)-16384
          JSGN   = ISHFT (IAND (MSKA32,IARRAY(J)),-16)
          JMANT  = IAND  (MSKA24,IARRAY(J))*J2(INDX)
          JDUM   = IOR   (ISHFT(JMANT,16),ISHFT(ISHFT(JMANT,9),-25))
          JDUM   = IOR   (IOR (JDUM,JEXP),JSGN)
          IF (JEXP.LT.   127) JDUM = ISMA
          IF (JEXP.GT. 32640) JDUM = IBIG
          IF (IARRAY(J).NE.0) IARRAY(J) = JDUM
   2    CONTINUE
      ENDIF

      END
