*
* $Id: xreads.F,v 1.1.1.1 1996/02/15 17:51:18 mclareni Exp $
*
* $Log: xreads.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:18  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XREADS (LUN,MVA,NA,MVB,NB)
 
C CERN PROGLIB# Z200    XREADS          .VERSION KERNCDC  1.05  780503
 
C-    ROUTINE TO READ 1 BINARY RECORD SPLIT INTO 2 PARTS, 29/04/78  JZ
 
      DIMENSION    MVA(NA),  MVB(NB)
 
 
      IF (NA.LE.0)           GO TO 31
      IF (NB.LE.0)           GO TO 24
      READ (LUN) MVA, MVB
      RETURN
 
   24 READ (LUN) MVA
      RETURN
 
   31 IF (NB.LE.0)           RETURN
      READ (LUN) MVB
      RETURN
      END
