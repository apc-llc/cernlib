*
* $Id: bufin.F,v 1.1.1.1 1996/01/11 14:15:04 mclareni Exp $
*
* $Log: bufin.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:04  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BUFIN(IL,IFLAG)
C     **************************
C-- ADAPTED FROM ISAJET
C
C          INVERSE OF BUFOUT.
C          READ ONE INPUT RECORD INTO ZEVEL.
C
C-- CREATED: 88/05/08
#include "cojets/itapes.inc"
#include "cojets/zevel.inc"
      DATA NPARR/0/
      ITB=ABS(ITEVT)
    1 CONTINUE
C          STANDARD FORTRAN 77 READ.
      READ(ITB,ERR=100,END=200) ZEVEL
C      IZVL1=ZEVEL(1)
C      IZVL2=ZEVEL(2)
      GO TO 300
C            TAPE READ ERROR
  100 WRITE(ITLIS,10) ITB
      NPARR=NPARR+1
   10 FORMAT(1X,' TAPE READ ERROR ON TAPE',I3)
      IFLAG=1
      IF(NPARR.LT.20) GOTO 1
C            END OF FILE
200   CONTINUE
      IFLAG=-1
      RETURN
C            GOOD RECORD
  300 IFLAG=0
      IL=3
      RETURN
      END
