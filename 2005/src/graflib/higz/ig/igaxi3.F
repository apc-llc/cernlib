*
* $Id: igaxi3.F,v 1.1.1.1 1996/02/14 13:10:33 mclareni Exp $
*
* $Log: igaxi3.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:33  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.16/04 09/10/92  13.48.31  by  O.Couet
*-- Author :
      SUBROUTINE IGAXI3(A1,A2,NAA,BL,BH,NB,BWID)
*.==========>
*.
*.   Adjust binsiz
*.
*..=========>
      AL=MIN(A1,A2)
      AH=MAX(A1,A2)
      IF(AL.EQ.AH)AH=AL+1.
*
*              IF NAA .EQ. -1 , program uses bwid input from
*              calling routine
*
      IF(NAA.EQ.-1)GOTO 80
   10 NA=MAX(NAA,2)
      IF(NA.LT.1)NA=1
*
*              Get nominal bin width in expon form
*
   20 AWID=(AH-AL)/FLOAT(NA)
      JLOG=LOG10(AWID)
      IF(AWID.LE.1.0)JLOG=JLOG-1
      SIGFIG=AWID*(10.00**(-JLOG))
*
*              Round mantissa up to 1, 2, 2.5, 5, or 10
*
      IF(SIGFIG.GT.1.0)GOTO 30
      SIGRND=1.0
      GOTO 70
   30 IF(SIGFIG.GT.2.0)GOTO 40
      SIGRND=2.0
      GOTO 70
   40 IF(SIGFIG.GT.2.5)GOTO 50
      SIGRND=2.5
      GOTO 70
   50 IF(SIGFIG.GT.5.0)GOTO 60
      SIGRND=5.0
      GOTO 70
   60 SIGRND=1.0
      JLOG=JLOG + 1
   70 CONTINUE
      BWID=SIGRND*10.0**JLOG
      GOTO 90
*
*              Get new bounds from new width BWID
*
   80 IF(BWID.LE.0.)GOTO 10
   90 CONTINUE
      ALB=AL/BWID
      LWID=ALB
      IF(ALB.LT.0.0)LWID=LWID-1
      BL=SIGRND*FLOAT(LWID)*10.**JLOG
      ALB=AH/BWID+1.00001
      KWID=ALB
      IF(ALB.LT.0.0)KWID=KWID-1
      BH=SIGRND*FLOAT(KWID)*10.**JLOG
      NB=KWID-LWID
      IF(NAA.GT.5)GOTO 100
      IF(NAA.EQ.-1)RETURN
*
*              Request for one bin is difficult case
*
      IF(NAA.GT.1.OR.NB.EQ.1)RETURN
      BWID=BWID*2.0
      NB=1
      RETURN
  100 IF(2*NB.NE.NAA)RETURN
      NA=NA+1
      GOTO 20
*
      END
