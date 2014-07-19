*
* $Id: ie3tos.F,v 1.1.1.1 1996/02/15 17:53:13 mclareni Exp $
*
* $Log: ie3tos.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:13  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
#if defined(CERNLIB_QMIBMVF)
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
#endif
      SUBROUTINE IE3TOS (MS,MT,NWDO,JBAD)
C
C CERN PROGLIB# M220    IE3TOS          .VERSION KERNIBM  2.26  890316
C
C 02/03/89  M.Roethlisberger/IBM + J.Zoll/CERN   Optimise / Vectorise

C-    Convert single precision for input with copy
C-    from source in IEEE to target in native data format
C-
C-    Reference: ZEBRA REFERENCE MANUAL book FZ, para. 3.03
C-
C-    IEEE Representation: sccc cccc cmmm mmmm mmmm mmmm mmmm mmmm
C-    IBM  Representation: sccc cccc mmmm mmmm mmmm mmmm mmmm mmmm
C-
C-    m:       Mant (IBM) = (00800000+ Mant (IE3))/J1 (Right-shifting)

C-    c:       Exp  (IBM) = (Exp (IE3)-126+J2)/4 + 64 =
C-                        = (Exp (IE3)+130+J2)/4
C-
C-    s:       Sign (IBM) = Sign (IE3)
C
C        Where J1 and J2 are both function of the 2 right-most bits
C        of I3E exponent.

      DIMENSION    MS (99)  , MT (99)
      DIMENSION    J1 (0:3) , J2 (0:3)

      EQUIVALENCE  (ITHA    , THA)

C     PARAMETER    (MSKA31 =   Z 7FFFFFFF)
      PARAMETER    (MSKA31 = 2147 483 647)
C     PARAMETER    (MSKA7F =   Z 7F000003)
      PARAMETER    (MSKA7F = 2130 706 432)
C     PARAMETER    (MSKA23 =   Z 007FFFFF)
      PARAMETER    (MSKA23 =    8 388 607)
C     PARAMETER    (MSKB24 =   Z 00800000)
      PARAMETER    (MSKB24 =    8 388 608)
C     PARAMETER    (MSKC18 =   Z 01800000)
      PARAMETER    (MSKC18 =   25 165 864)
C     PARAMETER    (IBADMS =   Z 7FFFFFFF)
      PARAMETER    (IBADMS = 2147 483 647)
C     PARAMETER    (IOVPMS =   Z 7FFFFFF0)
      PARAMETER    (IOVPMS = 2147 483 632)
C     PARAMETER    (IOVNMS =   Z FFFFFFF0)
      PARAMETER    (IOVNMS = -16)

      PARAMETER    (LVMIN  =  44)


      DATA (J1 (II),II=0,3) /
     +          4,          2,          1,          8/

      DATA (J2 (II),II=0,3) /

C-    Respectively 2, 1, 0 and 3 but 22 positions left-shifted.
C-         00800000,    00400000,    00000000,    00C00000
     +    8 388 608,   4 194 304,           0,  12 582 912/

#include "q_andor.inc"
#include "q_shift.inc"

      JBAD = 0

#if defined(CERNLIB_QMIBMVF)
      IF (NWDO.LT.LVMIN)      GO TO 336
C*VDIR: PREFER VECTOR

C----              Vector loop

      DO 334  JL=1,NWDO
         MT(JL) = IOR( IOR(

C Add 1 bit in front of I3E mantissa, shift it right (0-3 positions)
     +            IOR (IAND(MS(JL),MSKA23),MSKB24)
     +            /J1(ISHFTR(IAND(MS(JL),MSKC18),23))

C Shift I3E exponent 1 position to right, add 20800000 (130 22 positions
C left-shifted), add J2 (also shifted) and discard all non-IBM exponents
C bits (i.e. /4)
     +            ,IAND(ISHFTR (ISHFTL(MS(JL),1),2) + 545 259 520
     +            +J2(ISHFTR(IAND(MS(JL),MSKC18),23)),MSKA7F))
C Add sign bit
     +            ,IAND (MS(JL),NOT(MSKA31)) )
  334 CONTINUE
      GO TO 341
C*VDIR: PREFER SCALAR ON
#endif
C----              Scalar loop

  336 DO 339  JL=1,NWDO
         MT(JL) = IOR( IOR(
     +            IOR (IAND(MS(JL),MSKA23),MSKB24)
     +            /J1(ISHFTR(IAND(MS(JL),MSKC18),23))
     +            ,IAND(ISHFTR (ISHFTL(MS(JL),1),2) + 545 259 520
     +            +J2(ISHFTR(IAND(MS(JL),MSKC18),23)),MSKA7F))
     +            ,IAND (MS(JL),NOT(MSKA31)) )
  339 CONTINUE

C----              Second loop for exceptions handling

  341 DO 349  JL=1,NWDO

C Stored Exp (I3E)
      JEXP3 = ISHFTR (ISHFTL(MS(JL),1),24)
      IF (JEXP3.EQ.0) THEN
         JMAN3 = IAND (MS(JL), MSKA23)
         IF (JMAN3.EQ.0) THEN
C Exp=0 ; Mant=0    : Floating-point zero
            MT(JL)=0
          ELSE

C Exp=0 ; Mant ~= 0 : De-normalized number
            THA  = FLOAT(JMAN3) * 2.**(-63)
            THA  = THA * 2.**(-86)
            IF (IAND (MS(JL),NOT(MSKA31)).NE.0) THA=-THA
            MT(JL)=ITHA
          ENDIF
       ENDIF

      IF (JEXP3.EQ.255)  THEN
C Error conditions
         JBAD  = JL
         JMAN3 = IAND (MS(JL), MSKA23)
         IF (JMAN3.EQ.0)    THEN
C Exp=255 ; Mant = 0 : Signed infinity
            MT(JL) = IOR (IOVPMS, IAND(MS(JL),NOT(MSKA31)) )
          ELSE
C Exp = 255 ; Mant ~= 0 : Not a Number (NaN)
            MT(JL) = IBADMS
          ENDIF
       ENDIF
  349 CONTINUE
      RETURN
      END
#ifdef CERNLIB_TCGEN_IE3TOS
#undef CERNLIB_TCGEN_IE3TOS
#endif
