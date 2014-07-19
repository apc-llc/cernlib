*
* $Id: rend.F,v 1.1.1.1 1996/01/11 14:15:05 mclareni Exp $
*
* $Log: rend.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:05  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE REND
C     ***************
C-- ADAPTED FROM ISAJET
C
C          INVERSE OF WREND
C          READ END RECORD (TYPE 300)
C
C-- CREATED: 88/05/08
#include "cojets/final.inc"
#include "cojets/itapes.inc"
#include "cojets/zevel.inc"
C
C-- /FINAL/
      NKINF=IZEVEL(3)
      SIGF=ZEVEL(4)
      ALUM=ZEVEL(5)
      ACCEPT=IZEVEL(6)
      NRECS=IZEVEL(7)
      NEVTAP=IZEVEL(8)
      RETURN
      END
