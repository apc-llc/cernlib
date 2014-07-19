*
* $Id: check4.F,v 1.1.1.1 1996/02/15 17:48:46 mclareni Exp $
*
* $Log: check4.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:46  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE CHECK4(LWORK,W,NPK,OK,PKN,PKT)
          REAL      W(LWORK), PKT(*)
          LOGICAL   OK, OKPK
          CHARACTER*4 PKN(*)
#include "kernnumt/sysdat.inc"
          CHARACTER*4 LISTPK(2), NULL
          INTEGER   NREPPK(2)
          DATA      LISTPK
     +              /   'C205', 'NULL'/
*    +              /   'C205', 'C305', 'C308', 'C312', 'C313',
*    +                  'C336', 'C337', 'C341', 'NULL'  /
          DATA      NULL      /  'NULL'  /
          DATA      NREPPK    /  2, 0  /
*         DATA      NREPPK    /  2, 4, 2, 8,12, 4, 7, 5, 0  /
          DO 100    NPK  =  1, 100
             IF(LISTPK(NPK) .EQ. NULL)  GOTO 101
 100         CONTINUE
          NPK  =  0
          RETURN
 101      NPK  =  NPK - 1
          OK   =  .TRUE.
          DO 200    IPK  =  1, NPK
             NAMEPK  =  LISTPK(IPK)
             PKN(IPK)=  LISTPK(IPK)
             NREP    =  NREPPK(IPK)
             OKPK    =  .FALSE.
             WRITE(*,1000) NAMEPK
*             GOTO(01), IPK
*            GOTO(01,02,03,04), IPK
  01         CALL C205CH(NREP,OKPK)
             GOTO 90
  90         IF(.NOT. OKPK)  WRITE(*,1012) NAMEPK
             IF(      OKPK)  WRITE(*,1013) NAMEPK
             OK  =  OK .AND. OKPK
             CALL TIMEX( PKT(IPK) )
 200         CONTINUE
          IF(NPK .LT. 1)  OK  =  .FALSE.
          RETURN
1000      FORMAT(// 10H CHECK OF , A4,1H.)
1012      FORMAT(/ 5X, 24H ????? CHECK OF PACKAGE ,A4,
     +           18H HAS FAILED. ????? )
1013      FORMAT( 15X, 18H CHECK OF PACKAGE ,A4,12H SUCCESSFUL. )
          END
          SUBROUTINE FCTNCH
#include "kernnumt/sysdat.inc"
          REAL                RFUNC, TRUNC, ROUND
          REAL                RX, RY, RF, RE, DELTA, RFROMD
          DOUBLE PRECISION    DFUNC, X(*), Y(*), DX, DF
          LOGICAL             SAME, OK
          RFROMD(DX)  =  SNGL(DX+(DX-DBLE(SNGL(DX))))
          ENTRY DFEQDY(N,X,DFUNC,Y,TRUNC,ROUND,SAME)
          SAME  =  .TRUE.
          DELTA =  AMAX1(TRUNC,ROUND)
          DO 10     J  =  1, N
             DF    =  DFUNC(X(J))
             RE    =  SNGL(DABS( (DF-Y(J))/Y(J) ))
             OK    =  RE .LT. DELTA
             SAME  =  SAME .AND. OK
             IF(.NOT. OK)  WRITE(*,1000) J,X(J),Y(J),DF,RE,DELTA
  10         CONTINUE
          RETURN
          ENTRY RFEQDF(N,X,RFUNC,DFUNC,TRUNC,ROUND,SAME)
          SAME  =  .TRUE.
          DELTA =  AMAX1(TRUNC,ROUND)
          DO 20     J  =  1, N
             RX    =  RFROMD(X(J))
             DX    =  DBLE(RX)
             RY    =  RFROMD(DFUNC(DX))
             RF    =  RFUNC(RX)
             RE    =  ABS( (RF-RY)/RY )
             OK    =  RE .LT. DELTA
             SAME  =  SAME .AND. OK
             IF(.NOT. OK)  WRITE(*,1000) J,RX,RY,RF,RE,DELTA
  20         CONTINUE
          RETURN
          ENTRY RFEQDY(N,X,RFUNC,Y,TRUNC,ROUND,SAME)
          SAME  =  .TRUE.
          DELTA =  AMAX1(TRUNC,ROUND)
          DO 30     J  =  1, N
             RX    =  RFROMD(X(J))
             RY    =  RFROMD(Y(J))
             RF    =  RFUNC(RX)
             RE    =  ABS( (RF-RY)/RY )
             OK    =  RE .LT. DELTA
             SAME  =  SAME .AND. OK
             IF(.NOT. OK)  WRITE(*,1000) J,RX,RY,RF,RE,DELTA
  30         CONTINUE
          RETURN
1000    FORMAT(1P,/' ERROR FOR X(',I2,') =',E23.15,' ??? '/
     +     ' REF. VALUE =',E23.15,' DIFFERS FROM'/
     +     ' TEST VALUE =',E23.15,' BY THE REL. ERROR =',E10.2,/
     +     ' WHICH IS LARGER THAN THE MARGIN',17X,'DELTA =',E10.2)
          END
