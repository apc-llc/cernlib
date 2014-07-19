*
* $Id: zfield.F,v 1.1.1.1 1996/02/15 17:53:28 mclareni Exp $
*
* $Log: zfield.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:28  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION ZFIELD (X,Y,Z)
C
C
      COMMON/SLATE/BINS(34),COOR1,COOR(3),IADR,JADR
      COMMON/PHYS/PHYSIC(20)
      DIMENSION IPHYS(20)
      EQUIVALENCE (PHYSIC,IPHYS)
      DATA  FIELD/5HFIELD/, FIELD2/6HRFIELD/
C
C
      IADR=ITIT(FIELD)
      GOTO 1
C
         ENTRY RFIELD                                                    CDC
C        ENTRY RFIELD  (X,Y,Z)                                          -CDC
C
      IADR=ITIT(FIELD2)
C
    1 IF (IADR.EQ.0) GO TO 101
      IF (PHYSIC(IADR+1).EQ.-1.) GO TO 11
      COOR(1)=X-PHYSIC(IADR+8)
      COOR(2)=Y-PHYSIC(IADR+9)
      IF(COOR(1).EQ.0. .AND. COOR(2).EQ.0.)  GOTO 6
      IF (PHYSIC(IADR+1).EQ.0.) GO TO 6
      COOR1=SQRT (COOR(1)**2+COOR(2)**2)
      COOR(2)=57.29578    *ATAN2(COOR(2),COOR(1))
      IF(COOR(2) .LT. 0.) COOR(2)=COOR(2) + 360.
      COOR(1)=COOR1
    6 COOR(3)=Z-PHYSIC(IADR+10)
      JADR=11+IADR+IPHYS(IADR+5)+IPHYS(IADR+6)+IPHYS(IADR+7)
      ZFIELD=    PHYSIC(IADR+2)*FINT(3,COOR   ,IPHYS(IADR+5),PHYSIC(IADR
     1 +11),PHYSIC(JADR))+PHYSIC(IADR+3)
  100 RETURN
   11 ZFIELD=    PHYSIC(IADR+2)
C     RFIELD= ZFIELD                                                    IBM
      GO TO 100
  101 ZFIELD=0.
C     RFIELD= ZFIELD                                                    IBM
      GO TO 100
      END
