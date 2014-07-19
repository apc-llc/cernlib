*
* $Id: main.F,v 1.1.1.1 1996/01/11 14:15:06 mclareni Exp $
*
* $Log: main.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:06  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      PROGRAM MAIN
C     ************
C-- STEERS COJETS WITH CONVENTIONAL I/O INTERFACE
C-
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/bopar.inc"
#include "cojets/cutoff.inc"
#include "cojets/data1.inc"
#include "cojets/decpar.inc"
#include "cojets/eicht.inc"
#include "cojets/event.inc"
#include "cojets/evint.inc"
#include "cojets/evtype.inc"
#include "cojets/forgen.inc"
#include "cojets/fratab.inc"
#include "cojets/fstate.inc"
#include "cojets/itapes.inc"
#include "cojets/kdump.inc"
#include "cojets/keybre.inc"
#include "cojets/kfact.inc"
#include "cojets/khadro.inc"
#include "cojets/mb.inc"
#include "cojets/nflav.inc"
#include "cojets/nleave.inc"
#include "cojets/par.inc"
#include "cojets/parqua.inc"
#include "cojets/pthard.inc"
#include "cojets/stable.inc"
#include "cojets/tleave.inc"
#include "cojets/top.inc"
#include "cojets/total.inc"
#include "cojets/weakbo.inc"
#include "cojets/zwpar.inc"
C-
C- INSERT HERE INPUT CARDS AND COMMANDS
C-----------------------------------------------------------------------
C-
C-
C-----------------------------------------------------------------------
C-
      CALL GEVENT
C-
C- CALL PARTAB TO HAVE PARTICLE AND DECAY TABLE PRINTED
CCC   CALL PARTAB
C-
      STOP
      END
