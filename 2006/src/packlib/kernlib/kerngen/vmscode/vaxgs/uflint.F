*
* $Id: uflint.F,v 1.1.1.1 1996/02/15 17:50:25 mclareni Exp $
*
* $Log: uflint.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:25  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE UFLINT (VECT,NWP,MODEP)
C
C CERN PROGLIB# M251    UFLINT          .VERSION KERNVAX  2.09  840215
C ORIG. 22/07/82 JZ
C
C-    CONVERT VECT(1->NWP) IN SITU TO FLOATING OR INTEGERS ACCORDING
C-    TO MODEP
C
      DIMENSION    VECT(99), NWP(9), MODEP(9)
      EQUIVALENCE  (A,IA), (B,IB)
      LOGICAL      VARY

#include "kerngen/q_jbit.inc"

      NW  = NWP(1)
      MODE= MODEP(1)
      N   = 1 - NW
      VARY= .TRUE.
      IF (NW.LT.0)           GO TO 31
      N   = NW + 1
      VARY= .FALSE.
      JUSE= JBIT (MODE,1)
      GO TO 31

   30 VECT(N) = B
   31 N = N - 1
      IF (N.EQ.0)            RETURN
      IF (VARY)  JUSE=JBIT(MODE,N)
      A   = VECT(N)
      IF (JUSE.NE.0)         GO TO 36

C--                FLOATING WANTED

      JCH = JBYT (IA,8,8)
      IF (JCH.EQ.0)          GO TO 34
      IF (JCH.NE.255)        GO TO 31
   34 B = IA
      GO TO 30

C--                INTEGER WANTED

   36 JCH = JBYT (IA,25,8)
      IF (JCH.EQ.0)          GO TO 31
      IF (JCH.EQ.255)        GO TO 31
      IB = A
      GO TO 30
      END
