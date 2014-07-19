*
* $Id: steerd.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: steerd.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE STEERD
C.----------------------------------------------------------------------
C.
C.    STEERING OF TEST PROGRAM FOR EURODEC PARTICLE DECAYS.
C.    LOOP OVER PARTICLES IN THE PARTICLE TABLE AND LET THEM DECAY.
C.    (USES HBOOK LIBRARY ROUTINES!).
C.    LAST UPDATE: 01/03/89
C.
C.----------------------------------------------------------------------
C--
C--   BOOK HISTOGRAMS
      CALL HBOOK1(1,'CHARGED PARTICLE MULTIPLICITY IN DECAYS$',
     &60,0.,60.,0.)
      CALL HBOOK1(2,'NEUTRAL PARTICLE MULTIPLICITY IN DECAYS$',
     &60,0.,60.,0.)
C--
C--   INITIALIZE FRAGMENTATION AND DECAY PACKAGE...
      CALL EUDINI
C--
C--   TEST FULL FRAGMENTATION AND DECAY
      CALL TESTD
C--
C--   WRITE HISTOGRAMS TO FILE ON LOGICAL UNIT LUN2
      CALL HISTDO
      END
