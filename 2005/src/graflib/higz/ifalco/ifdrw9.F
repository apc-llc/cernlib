*
* $Id: ifdrw9.F,v 1.2 1996/03/07 17:38:09 couet Exp $
*
* $Log: ifdrw9.F,v $
* Revision 1.2  1996/03/07 17:38:09  couet
* IFALCO selection was wrong
*
* Revision 1.1.1.1  1996/02/14 13:10:30  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/08 18/01/94  14.33.00  by  O.Couet
*-- Author :
      SUBROUTINE IFDRW9(IX,IY)
#include "higz/hifc1.inc"
#include "higz/hifc6.inc"
      DATA IYLL,IXHL,IYXL/-1,-1,-1/
      IF (IX.LT.0) RETURN
      IF (IY.LT.0) RETURN
      IF (131071.LT.IX) RETURN
      IF (ISFL.EQ.0) RETURN
      IXN=IX/128
      IYN=IY/128
      IXH=IXN/32
      IXL=IXN-32*IXH
      IYH=IYN/32
      IYL=IYN-32*IYH
      CALL IFPUT(IYH+32)
      IF (ISCR.EQ.4) THEN
         IYX=4*((IY-128*IYN)/32)+(IX-128*IXN)/32
         IF (IYX.NE.IYXL) CALL IFPUT(IYX+96)
         IF (IYX.NE.IYXL.OR.IYL.NE.IYLL.OR.IXH.NE.IXHL) THEN
            CALL IFPUT(IYL+96)
            IYLL=IYL
         END IF
         IYXL=IYX
      ELSE
         IF (IYL.NE.IYLL.OR.IXH.NE.IXHL) THEN
            CALL IFPUT(IYL+96)
            IYLL=IYL
         END IF
      END IF
      IF (IXH.NE.IXHL) THEN
         CALL IFPUT(IXH+32)
         IXHL=IXH
      END IF
      CALL IFPUT(IXL+64)
      IF (240.LT.IPOUT) THEN
         CALL IFPUT(0)
         CALL IFPUT(29)
         CALL IFPUT(IYH+32)
         CALL IFPUT(IYL+96)
         CALL IFPUT(IXH+32)
         CALL IFPUT(IXL+64)
      END IF
      END
