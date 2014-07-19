*
* $Id: uflint.F,v 1.1.1.1 1996/02/15 17:54:17 mclareni Exp $
*
* $Log: uflint.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:17  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
#if !defined(CERNLIB_TCGEN)
      SUBROUTINE UFLINT (VECT,NWP,MODEP)
C
C CERN PROGLIB# M251    UFLINT          .VERSION KERNFOR  4.23  891215
C ORIG. 01/10/77 JZ
C
C-    CONVERT VECT(1->NWP) IN SITU TO FLOATING OR INTEGERS ACCORDING
C-    TO MODEP
C
C-    PROGRAM WRITTEN 12-AUG-77, JZ
C
      DIMENSION    VECT(99), NWP(9), MODEP(9)
      EQUIVALENCE  (A,IA), (B,IB)
      LOGICAL      VARY
C
#if defined(CERNLIB_QMCV64)
      PARAMETER    (JEX=53, NEX=12, NEG=4095 )
#endif
#if defined(CERNLIB_QMCRY)
      PARAMETER    (JEX=49, NEX=16, NEG=65535)
#endif
#if defined(CERNLIB_QMCDC)
      PARAMETER    (JEX=49, NEX=12, NEG=4095 )
#endif
#if defined(CERNLIB_QMUNI)||defined(CERNLIB_QMUNO)
      PARAMETER    (JEX=28, NEX= 9, NEG= 511 )
#endif
#if defined(CERNLIB_QMPDP)
      PARAMETER    (JEX=28, NEX= 9, NEG= 511 )
#endif
#if defined(CERNLIB_QMDGE)||defined(CERNLIB_QMIBM)
      PARAMETER    (JEX=25, NEX= 8, NEG= 255 )
#endif
#if (defined(CERNLIB_QIEEE))&&(defined(CERNLIB_B32))
      PARAMETER    (JEX=24, NEX= 9, NEG= 511 )
#endif
#if defined(CERNLIB_QMND3)
      PARAMETER    (JEX=23, NEX=10, NEG=1023 )
#endif
C
#include "kerngen/q_jbit.inc"
* Ignoring t=pass
C
C
      NW  = NWP(1)
      MODE= MODEP(1)
      N   = 1 - NW
      VARY= .TRUE.
      IF (NW.LT.0)           GO TO 31
      N   = NW + 1
      VARY= .FALSE.
      JUSE= JBIT (MODE,1)
      GO TO 31
C
   30 VECT(N) = B
   31 N = N - 1
      IF (N.EQ.0)            RETURN
      IF (VARY)  JUSE=JBIT(MODE,N)
      A   = VECT(N)
      JCH = JBYT (IA,JEX,NEX)
      IF (JCH.EQ.0)          GO TO 36
      IF (JCH.EQ.NEG)        GO TO 36
C
C--                CONVERT TO INTEGER
C
      IF (JUSE.EQ.0)         GO TO 31
      IB = A
      GO TO 30
C
C--                CONVERT TO FLOATING
   36 IF (JUSE.NE.0)         GO TO 31
      B = IA
      GO TO 30
      END
#endif
