*
* $Id: csicns.F,v 1.1.1.1 1996/02/26 17:16:20 mclareni Exp $
*
* $Log: csicns.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:20  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 17/02/94  16.13.20  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSICNS(J,N,NC)
***----------------------------------
*      gets character constant '.....''....'
***------------------------------
#include "comis/cspar.inc"
#include "comis/cspnts.inc"
#include "comis/cskeys.inc"
#include "comis/csichv.inc"
      PARAMETER (NCHAR=4)
      COMMON /MDPOOL/ IQ(6)
      JT=J
      NT=N
      N1=0
      IF(NT.LE.0) GO TO 3
    1 IF(MKCHAR(JT).NE.ICHQUO) GO TO 2
      NT=NT-1
      JT=JT+1
      N1=N1+1
      IF(NT.EQ.0) GO TO 4
      IF(MKCHAR(JT).NE.ICHQUO) GO TO 4
    2 JT=JT+1
      NT=NT-1
      N1=N1+1
      IF(NT.NE.0) GO TO 1
    3 CALL CSRD(JT,NT)
      IF(IFORS.NE.0 .AND. KEYC.NE.0)GO TO 1
      CSICNS=0
      RETURN
    4 J1=J
      N1=N1-1
      J=JT
      N=NT
      IF(N1.LE.0) THEN
*     mods for zero length constant ( '' )
        I=MHLOC(1)
        IQ(I)=KBLN
        NC=1
      ELSE
        I=MHLOC((N1-1)/NCHAR+1)
        JT=MJCHAR(IQ(I))
        NC=0
    5   K=MKCHAR(J1)
        J1=J1+1
        IF(K.NE.ICHQUO) GO TO 6
        J1=J1+1
        N1=N1-1
    6   CALL CSCHAR(JT,K)
        NC=NC+1
        JT=JT+1
        N1=N1-1
        IF(N1.NE.0) GO TO 5
      END IF
      CSICNS=I
      RETURN
      END
