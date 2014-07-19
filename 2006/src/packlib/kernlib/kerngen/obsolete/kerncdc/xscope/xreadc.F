*
* $Id: xreadc.F,v 1.1.1.1 1996/02/15 17:51:17 mclareni Exp $
*
* $Log: xreadc.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:17  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XREADC (LUN,MV,N)
 
C CERN PROGLIB# Z200    XREADC          .VERSION KERNCDC  1.13  820111
 
C-    ROUTINE TO READ 1 CODED RECORD,  29/04/78  JZ
 
      DIMENSION    MV(N)
 
      DATA  LBCD / 14 /
 
 
      IF (N.LE.LBCD)         GO TO 22
      READ (LUN,1000) (MV(J),J=1,LBCD)
      RETURN
 
   22 READ (LUN,1000) MV
      RETURN
 
 1000 FORMAT (13A10,A6)
      END
