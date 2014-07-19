*
* $Id: ie3fod.F,v 1.1.1.1 1996/02/15 17:53:13 mclareni Exp $
*
* $Log: ie3fod.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:13  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
#if defined(CERNLIB_QMIBMVF)
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
#endif
      SUBROUTINE IE3FOD (MS,MT,NDPN,JBAD)
C
C CERN PROGLIB# M220    IE3FOD          .VERSION KERNIBM  2.26  890316

C 02/03/89  M.Roethlisberger/IBM + J.Zoll/CERN   Optimise / Vectorise
C
C-    Convert double precision for output with copy
C-    from source in native to target in IEEE data format

C-    Reference: ZEBRA REFERENCE MANUAL book FZ, para. 3.03
C-
C-    IBM  Representation: sccc cccc mmmm mmmm mmmm mmmm mmmm mmmm
C-                         mmmm mmmm mmmm mmmm mmmm mmmm mmmm mmmm

C-    IEEE Representation: sccc cccc cccc mmmm mmmm mmmm mmmm mmmm
C-                         mmmm mmmm mmmm mmmm mmmm mmmm mmmm mmmm
C-
C-    m:      Mant (I3E) = Mant (IBM)/J1 (right-shifting)
C-                         and left-most bit discarded (hidden bit)

C-    c:      Exp  (I3E) = 4*(Exp (IBM) - 64) - 1019 + J2
C-                       = 4*Exp(IBM) - 763 + J2
C-
C-    s:      Sign (IBM) = Sign (IE3)
C-
C-       Where J1 and J2 are both function of the 4 left-most bits
C-       of IBM mantissa.
C-
C-
      DIMENSION    MS(99)   , MT(99)
      DIMENSION    J1(0:15) , J2(0:15) , J3(0:15)

C     PARAMETER    (IBADMS =   Z 7FFFFFFF)
      PARAMETER    (IBADMS = 2147 483 647)
C     PARAMETER    (MSKA31 =   Z 7FFFFFFF)
      PARAMETER    (MSKA31 = 2147 483 647)
C     PARAMETER    (MSKA7F =   Z 7F000000)
      PARAMETER    (MSKA7F = 2130 706 432)
C     PARAMETER    (MN2131 =   Z 800FFFFF)
      PARAMETER    (MN2131 =-2146 435 073)
C     PARAMETER    (MSKA24 =   Z 00FFFFFF)
      PARAMETER    (MSKA24 =   16 777 215)
C     PARAMETER    (MSK00F =   Z 00F00007)
      PARAMETER    (MSK00F =   15 728 640)
C     PARAMETER    (IBADCD =   Z 7FF01FC0)
      PARAMETER    (IBADCD = 2146 443 200)
C     PARAMETER    (IOVPCD =   Z 7FF00000)
      PARAMETER    (IOVPCD = 2146 435 072)
C     PARAMETER    (IOVNCD =   Z FFF00000)
      PARAMETER    (IOVNCD =   -1 048 576)

      PARAMETER    (LVMIN  =        38)

      DATA  (J1 (II),II=0,15) /
C
C II= 0000, 0001, 0010, 0011, 0100, 0101, 0110, 0111,
     +   1,    1,    2,    2,    4,    4,    4,    4,
C
C II= 1000, 1001, 1010, 1011, 1100, 1101, 1110, 1111,
     +   8,    8,    8,    8,    8,    8,    8,    8/

      DATA  (J2 (II),II=0,15) /
C
C II =      0000,       0001,       0010,       0011,
C J2 =  00000000,   00000000,   00100000,   00100000,
     +         0,          0,  1 048 576,  1 048 576,
C
C II =      0100,       0101,       0110,       0111,
C J2 =  00200000,   00200000,   00200000,   00200000,
     + 2 097 152,  2 097 152,  2 097 152,  2 097 152,
C
C II =      1000,       1001,       1010,       1011,
C J2 =  00300000,   00300000,   00300000,   00300000,
     + 3 145 728,  3 145 728,  3 145 728,  3 145 728,
C
C II =      1100,       1101,       1110,       1111
C J2 =  00300000,   00300000,   00300000,   00300000,
     + 3 145 728,  3 145 728,  3 145 728,  3 145 728/


      DATA  (J3 (II),II=0,15) /
C
C II= 0000, 0001, 0010, 0011, 0100, 0101, 0110, 0111,
     +   8,    8,    4,    4,    2,    2,    2,    2,
C
C II= 1000, 1001, 1010, 1011, 1100, 1101, 1110, 1111,
     +   1,    1,    1,    1,    1,    1,    1,    1/

#include "q_andor.inc"
#include "q_shift.inc"

      JBAD  = 0
      NLOOP = 2*NDPN

#if defined(CERNLIB_QMIBMVF)
C*VDIR: PREFER VECTOR
      IF (NDPN.LT.LVMIN)      GO TO 336

C----              Vector loop

      DO 334  JL=1,NLOOP,2

C Stores the 4 left-most bits of IBM mantissa
         JMOST    = ISHFTR (ISHFTL(MS(JL),8), 28)

C Stores I3E exponent
         JEXP     = ISHFTR (IAND(MS(JL),MSKA7F), 2)
     +              + J2(JMOST) + 800 063 488

C Stores I3E left part of mantissa
         JMANT    = IAND (IAND(MS(JL),MSKA24) / J1(JMOST), MN2131)

C Stores lost bits due to right-shifting
         LOST     = IOR (ISHFTL(MS(JL),29), ISHFTL (
     +              ISHFTR(MS(JL+1),31), 28) ) * J3(JMOST)

C Concatenates left part of I3E mantissa, exponent and sign
         MT(JL)   = IOR ( IOR(JMANT,JEXP),IAND(MS(JL),NOT(MSKA31)))

C Concatenates lost bits and right part of mantissa
         MT(JL+1) = IOR (LOST,IAND (MS(JL+1),MSKA31)/J1(JMOST) )
  334 CONTINUE
      GO TO 341
C*VDIR: PREFER SCALAR ON
#endif
C----              Scalar loop

  336 DO 339  JL=1,NLOOP,2
         JMOST    = ISHFTR (ISHFTL(MS(JL),8), 28)
         JEXP     = ISHFTR (IAND(MS(JL),MSKA7F), 2)
     +              + J2(JMOST) + 800 063 488
         JMANT    = IAND (IAND(MS(JL),MSKA24) / J1(JMOST), MN2131)
         LOST     = IOR (ISHFTL(MS(JL),29), ISHFTL (
     +              ISHFTR(MS(JL+1),31), 28) ) * J3(JMOST)
         MT(JL)   = IOR ( IOR(JMANT,JEXP),IAND(MS(JL),NOT(MSKA31)))
         MT(JL+1) = IOR (LOST,IAND (MS(JL+1),MSKA31)/J1(JMOST) )
  339 CONTINUE

C----              Second loop for exceptions handling

  341 DO 349  JL=1,NLOOP,2
      IF (IAND (MS(JL),MSK00F).EQ.0) THEN
C Floating point zero/underflow
         MT(JL  ) = 0
         MT(JL+1) = 0
         GO TO 349
       ENDIF

      IF (IAND (MS(JL),MSKA7F).EQ.MSKA7F) THEN
C Error conditions
         JBAD = (JL+1) / 2
         IF (MS(JL).NE.IBADMS)  THEN
C Signed infinity
            MT(JL)   = IOR (IOVPCD, IAND(MS(JL),NOT(MSKA31)) )
            MT(JL+1) = 0
          ELSE
C Not a Number (NaN)
            MT(JL)   = IBADCD
            MT(JL+1) = 0
          ENDIF
       ENDIF
  349 CONTINUE
      RETURN
      END
#ifdef CERNLIB_TCGEN_IE3FOD
#undef CERNLIB_TCGEN_IE3FOD
#endif
