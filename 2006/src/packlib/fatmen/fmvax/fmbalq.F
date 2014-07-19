*
* $Id: fmbalq.F,v 1.1.1.1 1996/03/07 15:18:21 mclareni Exp $
*
* $Log: fmbalq.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:21  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      INTEGER FUNCTION FMBALQ(FAB,RAB,LUN)
*CMZ :          25/09/91  16.24.40  by  Jamie Shiers
*-- Author :    Jamie Shiers   25/09/91
      IMPLICIT INTEGER S
      INCLUDE '($FABDEF)'
      INCLUDE '($RABDEF)'
      INCLUDE '($RMSDEF)'
 
      RECORD /FABDEF/ FAB
      RECORD /RABDEF/ RAB
 
#include "fatmen/fabalq.inc"
 
      INTEGER LUN,RECFM
 
      RECFM = FAB.FAB$B_RFM
 
      STATUS = SYS$OPEN(FAB)
      IF(.NOT.STATUS) GOTO 99
 
      NBLOKS = FAB.FAB$L_ALQ
 
      FMBALQ = 0
      RETURN
 
99    FMBALQ = 1
      END
