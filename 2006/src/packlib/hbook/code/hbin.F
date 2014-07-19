*
* $Id: hbin.F,v 1.1.1.1 1996/01/16 17:07:32 mclareni Exp $
*
* $Log: hbin.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:32  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          30/06/95  09.31.39  by  Rene Brun
*-- Author :
      SUBROUTINE HBIN(A1,A2,NAA,BL,BH,NB,BWID)
*.==========>
*.           adjust binsiz
*..=========> ( F.James )
      AL = MIN(A1,A2)
      AH = MAX(A1,A2)
      IF (AL.EQ.AH) then
         if (ah.eq.0)then
            ah=1.
         else
            AH = AL + abs(al)*1.e-6
         endif
      endif
*         IF NAA .EQ. -1 , PROGRAM USES BWID INPUT FROM CALLING ROUTINE
      IF (NAA .EQ. -1)  GO TO 150
   10 NA = NAA - 1
      IF (NA .LT. 1)  NA = 1
*          GET NOMINAL BIN WIDTH IN EXPON FORM
   20 AWID = (AH-AL)/FLOAT(NA)
      JLOG = LOG10(AWID)
      IF (AWID .LE. 1.0)  JLOG=JLOG-1
      SIGFIG = AWID * (10.00 **(-JLOG))
*         ROUND MANTISSA UP TO 2, 2.5, 5, OR 10
      IF(SIGFIG .GT. 2.0)  GO TO 40
      SIGRND = 2.0
      GO TO 100
   40 IF (SIGFIG .GT. 2.5)  GO TO 45
      SIGRND = 2.5
      GO TO 100
   45 IF(SIGFIG.GT.4.)GO TO 50
      SIGRND=4.
      GO TO 100
   50 IF(SIGFIG .GT. 5.0)  GO TO 60
      SIGRND =5.0
      GO TO 100
   60 SIGRND = 1.0
      JLOG = JLOG + 1
  100 CONTINUE
      BWID = SIGRND*10.0**JLOG
      GO TO 200
*         GET NEW BOUNDS FROM NEW WIDTH BWID
  150 IF (BWID .LE. 0.)  GO TO 10
  200 CONTINUE
      ALB = AL/BWID
      LWID=ALB
      IF (ALB .LT. 0.0)  LWID=LWID-1
      BL=SIGRND*FLOAT(LWID)*10.**JLOG
      ALB = AH/BWID + 1.0
      KWID = ALB
      IF (ALB .LT. 0.0)  KWID=KWID-1
      BH=SIGRND*FLOAT(KWID)*10.**JLOG
      NB = KWID-LWID
      IF (NAA .GT. 5)  GO TO 240
      IF (NAA .EQ. -1)  RETURN
*          REQUEST FOR ONE BIN IS DIFFICULT CASE
      IF (NAA .GT. 1 .OR. NB .EQ. 1)  RETURN
      BWID =  BWID*2.0
      NB = 1
      RETURN
  240 IF (2*NB .NE. NAA)  RETURN
      NA = NA + 1
      GO TO 20
      END
