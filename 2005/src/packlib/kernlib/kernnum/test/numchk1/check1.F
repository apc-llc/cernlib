*
* $Id: check1.F,v 1.1.1.1 1996/02/15 17:48:41 mclareni Exp $
*
* $Log: check1.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:41  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE CHECK1(LWORK,W,NPK,OK,PKN,PKT)
          REAL      W(LWORK), PKT(*)
          CHARACTER*4 PKN(*)
          LOGICAL   OK, OKPK
#include "kernnumt/sysdat.inc"
          CHARACTER*4 LISTPK(5), NULL
          INTEGER   NREPPK(5)
          DATA      LISTPK
     +              /  'E100', 'E104', 'E105', 'E208', 'NULL'  /
          DATA      NULL      /  'NULL'  /
          DATA      NREPPK    /  21, 100, 100, 20, 0  /
*         DATA      NREPPK    /  21, 100, 100, 20, 8, 0  /
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
             GOTO(01,02,03,04), IPK
  01         NP=21
             NQ=21
             LP=1
             LQ=LP+NP
             IF(LQ+NP-1 .GT. LWORK) GO TO 900
             CALL E100CH(NP,W(LP),NQ,W(LQ),NREP,OKPK)
             GOTO 90
  02         NA=15
             NT=125
             LX=1
             LY=LX+NA
             IF(LY+NT-1 .GT. LWORK) GO TO 900
             CALL E104CH(NA,W(LX),NT,W(LY),OKPK)
             GOTO 90
  03         NA=25
             LA=1
             LB=LA+NA
             LF=LB+NA
             IF(LF+NA-1 .GT. LWORK) GO TO 900
             CALL E105CH(NREP,NA,W(LA),W(LB),W(LF),OKPK)
             GOTO 90
  04         NDIM  =  30
             MDIM  =  5
             LX    =  1
             LY    =  LX+NDIM
             LA    =  LY+NDIM
             LC    =  LA+MDIM
             IF(LC+MDIM-1 .GT. LWORK)  GOTO 900
             CALL E208CH(NDIM,W(LX),W(LY),MDIM,W(LA),W(LC),NREP,OKPK)
             GO TO 90
  90         IF(.NOT. OKPK)  WRITE(*,1012) NAMEPK
             IF(      OKPK)  WRITE(*,1013) NAMEPK
             OK  =  OK .AND. OKPK
             CALL TIMEX( PKT(IPK) )
 200         CONTINUE
          IF(NPK .LT. 1)  OK  =  .FALSE.
          RETURN
 900      OK  =  .FALSE.
          WRITE(*,1020)
          RETURN
1000      FORMAT(// 10H CHECK OF , A4,1H.)
1012      FORMAT(/ 5X, 24H ????? CHECK OF PACKAGE ,A4,
     +           18H HAS FAILED. ????? )
1013      FORMAT( 15X, 18H CHECK OF PACKAGE ,A4,12H SUCCESSFUL. )
1020      FORMAT(// 42H ????? CHECK 3 HAS INSUFFICIENT WORK SPACE )
          END
