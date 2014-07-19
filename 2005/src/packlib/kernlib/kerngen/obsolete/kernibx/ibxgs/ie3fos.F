*
* $Id: ie3fos.F,v 1.1.1.1 1996/02/15 17:54:36 mclareni Exp $
*
* $Log: ie3fos.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:36  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
#if defined(CERNLIB_QMIBXVF)
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
#endif
      SUBROUTINE IE3FOS (MS,MT,NWDO,JBAD)
C
C CERN PROGLIB# M220    IE3FOS          .VERSION KERNIBX  1.06  940224
C
C 02/03/89  M.Roethlisberger/IBM + J.Zoll/CERN   Optimise / Vectorise
C 01/11/90  M.Roethlisberger/IBM   Explicit sectioning, Bug fixed
C                                  (null mantissa implies now MT=0)

C-    Convert single precision for output with copy
C-    from source in native to target in IEEE data format
C-
C-    Reference: ZEBRA REFERENCE MANUAL book FZ, para. 3.03
C-
C-    IBM  Representation:  sccc cccc mmmm mmmm mmmm mmmm mmmm mmmm
C-    IEEE Representation:  sccc cccc cmmm mmmm mmmm mmmm mmmm mmmm

C-    m:      Mant(I3E)  = Mant (IBM)*J1 (left-shifting)
C-                         and discards left-most bit (hidden bit)

C-    c:      Exp  (I3E) = 4*(Exp(IBM)-64) + J2 + 126 =
C-                       = 4*EXP(IBM) + J2 -130

C-    s:      Sign (I3E) = Sign(IBM)
C-
C-       Where J1 and J2 are both function of the 4 leftmost bits
C-       of IBM mantissa.
C-
#if defined(CERNLIB_3090S)||defined(CERNLIB_3090J)
      PARAMETER    (IS     =   256)
#endif
#if (!defined(CERNLIB_3090S))&&(!defined(CERNLIB_3090J))
      PARAMETER    (IS     =   128)
#endif
      DIMENSION    MS(*)     , MT(*)
      DIMENSION    J1 (0:15) , J2 (0:15)
      DIMENSION    JEXP(IS)  , INP(IS)

C--   PARAMETER    (IBADMS =   Z 7FFFFFFF)
      PARAMETER    (IBADMS = 2147 483 647)
C--   PARAMETER    (MSKA7F =   Z 7F000000)
      PARAMETER    (MSKA7F = 2130 706 432)
C--   PARAMETER    (MSKA23 =   Z 007FFFFF)
      PARAMETER    (MSKA23 =    8 388 607)
C--   PARAMETER    (MSKA24 =   Z 00FFFFFF)
      PARAMETER    (MSKA24 =   16 777 215)
C--   PARAMETER    (MSKA31 =   Z 7FFFFFFF)
      PARAMETER    (MSKA31 = 2147 483 647)
C--   PARAMETER    (IBADCS =   Z 7F80FE00)
      PARAMETER    (IBADCS = 2139 160 064)
C--   PARAMETER    (IOVPCS =   Z 7F800000)
      PARAMETER    (IOVPCS = 2139 095 040)
C--   PARAMETER    (IOVNCS =   Z FF800000)
      PARAMETER    (IOVNCS =   -8 388 608)

      PARAMETER    (M130   = -130        )
      PARAMETER    (LVMIN  =   30        )

      DATA  (J1 (II),II=0,15) /
C
C II= 0000, 0001, 0010, 0011, 0100, 0101, 0110, 0111,
     +  16,    8,    4,    4,    2,    2,    2,    2,
C
C II= 1000, 1001, 1010, 1011, 1100, 1101, 1110, 1111,
     +   1,    1,    1,    1,    1,    1,    1,    1/


      DATA  (J2 (K),K=0,15) /
C
C II= 0000, 0001, 0010, 0011, 0100, 0101, 0110, 0111,
     +  -4,   -3,   -2,   -2,   -1,   -1,   -1,   -1,
C
C II= 1000, 1001, 1010, 1011, 1100, 1101, 1110, 1111
     +   0,    0,    0,    0,    0,    0,    0,    0/


      ISHFTL (IZW,NZB) = ISHFT (IZW, NZB)
      ISHFTR (IZW,NZB) = ISHFT (IZW,-NZB)

      JBAD = 0

      DO 100 I = 1,NWDO,IS
#if defined(CERNLIB_QMIBMVF)
        IF (NWDO-I+1.LT.LVMIN)      GO TO 336
C*VDIR: PREFER VECTOR

C----              Vector loop


        DO 334 JL=I,MIN0(NWDO,I+IS-1)
          JEXP (JL-I+1) = ISHFTR (IAND (MS(JL),MSKA7F),22)
     +    + J2 (ISHFTR (IAND (MS(JL),MSKA24),20) )
          INP  (JL-I+1) = MS (JL)
          J4B           = ISHFTR (IAND (MS(JL),MSKA24),20)

          MT (JL) = IOR (IOR

C Shift left IBM mantissa and discards the left-most significant bit
C (hidden bit)
     +            (IAND (MSKA23,IAND (MS(JL),MSKA24)
     +            * J1 (J4B)),

C Multiply IBM exponent by 4, add J2, add -130 and shift it
C back 23 positions left.
     +            ISHFTL(JEXP(JL-I+1) + M130,23) )

C Add sign bit
     +            ,IAND (MS(JL),NOT(MSKA31)))
          IF (J4B.EQ.0) MT(JL) = 0
  334   CONTINUE
        GO TO 341
C*VDIR: PREFER SCALAR ON
#endif
C----              Scalar loop

  336   DO 339 JL=I,MIN0(NWDO,I+IS-1)
          JEXP (JL-I+1) = ISHFTR (IAND (MS(JL),MSKA7F),22)
     +                    + J2 (ISHFTR (IAND (MS(JL),MSKA24),20) )
          INP  (JL-I+1) = MS (JL)
          J4B           = ISHFTR (IAND (MS(JL),MSKA24),20)

          MT (JL) = IOR (IOR
     +            (IAND (MSKA23,IAND (MS(JL),MSKA24)
     +            * J1 (J4B)),
     +            ISHFTL(JEXP(JL-I+1) + M130,23) )
     +            ,IAND (MS(JL),NOT(MSKA31)))
          IF (J4B.EQ.0) MT(JL) = 0

  339   CONTINUE


C----              Second loop for exceptions handling

  341   DO 349 JL=I,MIN0(NWDO,I+IS-1)

C Stored Exp (IBM) + 130

        IF (JEXP(JL-I+1).LE.130) THEN

C Exp (IBM) <= 0
         IF (JEXP(JL-I+1).GT.107) THEN

C Exp (IBM) > - 23.and. <= 0: 'De-normalized number'
            MT(JL) = ISHFTR ( IAND (INP(JL-I+1),MSKA24)
     +             * J1 (ISHFTR (IAND (INP(JL-I+1),MSKA24),20)),
     +            131-JEXP(JL-I+1))
     +            + IAND (INP(JL-I+1),NOT(MSKA31))
          ELSE

C Exp (IBM) <= -23 : floating point zero/underflow
            MT(JL) = 0
          ENDIF
       ENDIF

      IF (JEXP(JL-I+1).GE.385) THEN
C Exp (IBM) >= 255 : Error conditions, Overflow/NaN
         JBAD = JL
         IF (INP(JL-I+1).NE.IBADMS)  THEN
C Signed infinity
            MT(JL) = IOR (IOVPCS, IAND(INP(JL-I+1),NOT(MSKA31)) )
          ELSE
            MT(JL) = IBADCS
          ENDIF
       ENDIF
  349 CONTINUE
  100 CONTINUE
      RETURN
      END
#ifdef CERNLIB_TCGEN_IE3FOS
#undef CERNLIB_TCGEN_IE3FOS
#endif
