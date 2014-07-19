*
* $Id: repdmp2.F,v 1.1.1.1 1996/02/15 17:51:36 mclareni Exp $
*
* $Log: repdmp2.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:36  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE REPDMP2 (LUNP,MEM,NTOT,LADRP)
 
C CERN PROGLIB# Z038    REPDMP2         .VERSION KERNCDC  1.13  820111
 
      COMMON /EXCHPAK/JERRNO(2),LREP,AB(8),X(8),PARITY,USER
      DIMENSION MEM(99)
 
 
      LUN = LUNP
      WRITE (LUN,9000) JERRNO
      WRITE (LUN,9001) (AB(J),X(J),J=1,8)
      IF (NTOT.EQ.0)  RETURN
 
      WRITE (LUN,9012)
      LADR = LADRP
      DO 19 LA=1,NTOT,4
      LE = LA+3
      WRITE (LUN,9016) LADR,(MEM(J),J=LA,LE),(MEM(J),J=LA,LE)
   19 LADR= LADR + 4
      RETURN
 
 9000 FORMAT (22H0REPRIEVED   CONDITION,I4,A12/
     + 40H0   P----.A----.B----.  X    -    +    -)
 9001 FORMAT (2O22)
 9012 FORMAT (9H0SCM DUMP/1X)
 9016 FORMAT (1X,O6,4O21,4A11)
      END
