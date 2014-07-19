*
* $Id: ie3tod.F,v 1.1.1.1 1996/02/15 17:54:36 mclareni Exp $
*
* $Log: ie3tod.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:36  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
#if defined(CERNLIB_QMIBXVF)
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
#endif
      SUBROUTINE IE3TOD (MS,MT,NDPN,JBAD)
C
C CERN PROGLIB# M220    IE3TOD          .VERSION KERNIBX  1.00  900101

C 02/03/89  M.Roethlisberger/IBM + J.Zoll/CERN   Optimise / Vectorise
C
C-    Convert double precision for input with copy
C-    from source in IEEE to target in native data format
C-
C-    Reference: ZEBRA REFERENCE MANUAL book FZ, para. 3.03
C-
C-    IEEE Representation: sccc cccc cccc mmmm mmmm mmmm mmmm mmmm
C-                         mmmm mmmm mmmm mmmm mmmm mmmm mmmm mmmm
C-
C-    IBM  Representation: sccc cccc mmmm mmmm mmmm mmmm mmmm mmmm
C-                         mmmm mmmm mmmm mmmm mmmm mmmm mmmm mmmm

C-    m:      Mant (IBM) = (00100000 + Mant (IE3))*J1 (left-shifting)

C-    c:      Exp  (IBM) = (Exp (IE3)-1019+J2)/4 + 64 =
C-                       = (Exp (IE3)- 763+J2)/4
C-
C-    s:      Sign (IBM) = Sign (IE3)
C
C        Where J1 and J2 are both function of the 2 right-most bits
C        of I3E exponent
C
      DIMENSION    MS(99), MT(99)
      DIMENSION    J1(0:3) , J2(0:3), J3(0:3)

C     PARAMETER    (IBADMS =   Z 7FFFFFFF)
      PARAMETER    (IBADMS = 2147 483 647)
      PARAMETER    (JEXMIN = -259,  JEXMAX=251)
C     PARAMETER    (IOVPMS =   Z 7FFFFFF0)
      PARAMETER    (IOVPMS = 2147 483 632)
C     PARAMETER    (IOVNMS =   Z FFFFFFF0)
      PARAMETER    (IOVNMS = -16)
C     PARAMETER    (MSKA31 =   Z 7FFFFFFF)
      PARAMETER    (MSKA31 = 2147 483 647)
C     PARAMETER    (MSKA20 =   Z 000FFFFF)
      PARAMETER    (MSKA20 =    1 048 575)
C     PARAMETER    (MSKB21 =   Z 00100000)
      PARAMETER    (MSKB21 =    1 048 576)
C     PARAMETER    (MSK7FF =   Z 7FF00000)
      PARAMETER    (MSK7FF = 2146 435 072)
C     PARAMETER    (MSKC7F =   Z 7F000000)
      PARAMETER    (MSKC7F = 2130 706 432)
C     PARAMETER    (MSKC03 =   Z 00300000)
      PARAMETER    (MSKC03 =    3 145 728)
      PARAMETER    (IBADMD = IBADMS, IBADME = 0)
      PARAMETER    (IOVPMD = IOVPMS, IOVNMD = IOVNMS)
      PARAMETER    (M763SH = -800 063 488      )

      PARAMETER    (LVMIN  =       41    )


      DATA (J1 (II),II=0,3) /
     +            2,           4,           8,         1/

      DATA (J2 (II),II=0,3) /

C     Respectively -1, -2, -3 and 0 but 20 positions left-shifted
C          FFF00000,    FFE00000,    FFD00000,  00000000
     +   -1 048 576,  -2 097 152,  -3 145 728,         0/

      DATA (J3 (II),II=0,3) /
     +            4,           2,           1,         8/

#include "q_andor.inc"
#include "q_shift.inc"

      JBAD  = 0
      NLOOP = 2*NDPN

#if defined(CERNLIB_QMIBXVF)
      IF (NDPN.LT.LVMIN)      GO TO 336
C*VDIR: PREFER VECTOR

C----              Vector loop

      DO 334  JL=1,NLOOP,2

C Stores the 2 right-most bits of I3E exponent
         JMOST    = ISHFTR (IAND (MS(JL),MSKC03),20)

C Stores IBM exponent
         JXP      = IAND (ISHFTL ((IAND (MS(JL), MSK7FF)
     +              + J2(JMOST) + M763SH),2) ,MSKC7F)

C Stores IBM left part of mantissa
         JMAN     = (IOR(IAND (MS(JL),MSKA20),MSKB21))* J1(JMOST)

C Stores lost bits due to left-shifting of mantissa
         LOST     = ISHFTR (MS(JL+1),29)/J3(JMOST)

C Concatenates lost bits, left part of mantissa, exponent, and sign
         MT(JL)   = IOR (IOR (IOR (LOST,JMAN),JXP),
     +              IAND (MS(JL),NOT(MSKA31)) )

C Shift left the right part of IBM mantissa
         MT(JL+1) = (MS(JL+1)*J1(JMOST))
  334 CONTINUE
      GO TO 341
C*VDIR: PREFER SCALAR ON
#endif
C----              Scalar loop

  336 DO 339  JL=1,NLOOP,2
         JMOST    = ISHFTR (IAND (MS(JL),MSKC03),20)
         JXP      = IAND (ISHFTL ((IAND (MS(JL), MSK7FF)
     +              + J2(JMOST) + M763SH),2) ,MSKC7F)
         JMAN     = (IOR(IAND (MS(JL),MSKA20),MSKB21))* J1(JMOST)
         LOST     = ISHFTR (MS(JL+1),29)/J3(JMOST)
         MT(JL)   = IOR (IOR (IOR (LOST,JMAN),JXP),
     +              IAND (MS(JL),NOT(MSKA31)) )
         MT(JL+1) = (MS(JL+1)*J1(JMOST))
  339 CONTINUE

C----              Second loop for exceptions handling

  341 DO 349  JL=1,NLOOP,2

      IF (MS(JL).EQ.0) THEN
C Floating Point zero
         MT(JL)   = 0
         MT(JL+1) = 0
         GOTO 349
       ENDIF

C Stored Exp (I3E) - 1023
      JEXP = ISHFTR (IAND(MS(JL), MSK7FF), 20)-1023
      IF (JEXP.LT.JEXMIN) THEN
C Underflow
         MT(JL)   = 0
         MT(JL+1) = 0
         GOTO 349
       ENDIF

      IF (JEXP.GT.JEXMAX) THEN
C Error conditions
         JBAD = (JL+1) / 2
         IF (JEXP.NE.1024)  THEN
C Exp > 251 : Signed infinity
            MT(JL)   = IOR (IOVPMD, IAND(MS(JL),NOT(MSKA31)) )
            MT(JL+1) = 0
            GO TO 349
          ENDIF

         IF (IAND(MS(JL),MSKA20).EQ.0)    THEN
C Exp = 1024 ; Mant = 0 : Signed infinity
            MT(JL)   = IOR (IOVPMD, IAND(MS(JL),NOT(MSKA31)) )
            MT(JL+1) = 0
          ELSE
C Exp = 1024 ; mant ~= 0 : Not a number (NaN)
            MT(JL)   = IBADMD
            MT(JL+1) = 0
          ENDIF
       ENDIF
  349 CONTINUE
      RETURN
      END
#ifdef CERNLIB_TCGEN_IE3TOD
#undef CERNLIB_TCGEN_IE3TOD
#endif
