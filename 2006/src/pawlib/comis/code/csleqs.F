*
* $Id: csleqs.F,v 1.1.1.1 1996/02/26 17:16:19 mclareni Exp $
*
* $Log: csleqs.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:19  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/02 08/07/93  18.19.06  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION CSLEQS(J1,N1,J2,N2)
***----------------------------------
*     compares two strings (J1,N1) and (J2,N2)
***--------------------------
      INTEGER CSLEQS,CSLNES,CSLLTS,CSLLES,CSLGES,CSLGTS
#include "comis/cspar.inc"
#include "comis/csrec.inc"
#include "comis/cspnts.inc"
      KEY=1
      GO TO 7
         ENTRY CSLNES(J1,N1,J2,N2)
***--------------------------------
      KEY=2
      CSLNES=0
      GO TO 7
         ENTRY CSLLTS(J1,N1,J2,N2)
***--------------------------------
      KEY=3
      CSLLTS=0
      GO TO 7
        ENTRY CSLLES(J1,N1,J2,N2)
***-------------------------------
      KEY=4
      CSLLES=0
      GO TO 7
         ENTRY CSLGES(J1,N1,J2,N2)
***-------------------------------
      KEY=5
      CSLGES=0
      GO TO 7
         ENTRY CSLGTS(J1,N1,J2,N2)
***--------------------------------
      KEY=6
      CSLGTS=0
 7    IF(N1.GT.80.OR.N2.GT.80)THEN
        L1=N1
        L2=N2
        JJ1=J1
        JJ2=J2
        IE=1
        IG=0
        IL=0
        N=MAX0(N1,N2)
  17    IF(IE.EQ.1 .AND. N.GT.0)THEN
          REC=' '
          REC1=' '
          IF(L1.GT.0)THEN
             L=MIN0(L1,80)
             CALL CCOPYS(JJ1,JSR,L)
             JJ1=JJ1+L
             L1=L1-L
          ENDIF
          IF(L2.GT.0)THEN
             L=MIN0(L2,80)
             CALL CCOPYS(JJ2,JSR1,L)
             JJ2=JJ2+L
             L2=L2-L
          ENDIF
          IF(REC.GT.REC1)THEN
            IE=0
            IG=1
          ELSEIF(REC.LT.REC1)THEN
            IE=0
            IL=1
          ENDIF
          N=N-80
          GO TO 17
        ENDIF
        LF=0
        GO TO (11,12,13,14,15,16),KEY
   11   LF=IE
        GO TO 18
  12    IF(IE.EQ.0)LF=1
        GO TO 18
   13  LF=IL
       GO TO 18
  14  LF=IE+IL
       GO TO 18
  15  LF=IE+IG
      GO TO 18
  16  LF=IG
  18    CONTINUE
      ELSE
         CALL CCOPYS(J1,JSR,N1)
         CALL CCOPYS(J2,JSR1,N2)
         LF=0
         GO TO (1,2,3,4,5,6),KEY
   1  IF(REC(1:N1).EQ.REC1(1:N2))LF=1
        GO TO 8
   2  IF(REC(1:N1).NE.REC1(1:N2))LF=1
        GO TO 8
   3  IF(REC(1:N1).LT.REC1(1:N2))LF=1
        GO TO 8
   4  IF(REC(1:N1).LE.REC1(1:N2))LF=1
        GO TO 8
   5  IF(REC(1:N1).GE.REC1(1:N2))LF=1
        GO TO 8
   6  IF(REC(1:N1).GT.REC1(1:N2))LF=1
   8    CONTINUE
      ENDIF
      CSLEQS=LF
      END
