*
* $Id: check2.F,v 1.1.1.1 1996/02/15 17:48:41 mclareni Exp $
*
* $Log: check2.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:41  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE CHECK2(LWORK,W,NPK,OK,PKN,PKT)
          REAL      W(LWORK), PKT(*)
          LOGICAL   OK, OKPK
          CHARACTER*4 PKN(*)
#include "kernnumt/sysdat.inc"
          CHARACTER*4 LISTPK(6), NULL
          INTEGER   NREPPK(6)
          DATA      LISTPK
     +              /   'D509', 'D703', 'D704', 'F010', 'F406',
     +                  'NULL'/
*    +              /   'C204', 'C300', 'D103', 'D106', 'D209',
*    +                  'D509', 'D703', 'D704', 'F010', 'F406',
*    +                  'NULL'  /
          DATA      NULL      /  'NULL'  /
          DATA      NREPPK    /  5*1, 0  /
*         DATA      NREPPK    /  10*1, 0  /
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
             GOTO(01,02,03,04,05), IPK
   01        CALL D509CH(NREP,OKPK)
             GOTO 90
   02        NY  =  1026
             IF(NY .GT. LWORK)  GOTO 900
             CALL D703CH(NY,W,NREP,OKPK)
             GOTO 90
   03        NDIM  =  256
             LIMT  =  4*NDIM
             IF(LIMT .GT. LWORK)  GOTO 900
             CALL D704CH(NDIM,W(1),W(2*NDIM+1),NREP,OKPK)
#if defined(CERNLIB_NUMTIME)
                KNT = 4
                NDIM = 4096
                LIMT = 2*NDIM
                IF(LIMT .GT. LWORK) GOTO 800
                CALL D704TM(KNT,NDIM,W(1))
#endif
             GOTO 90
   04        NMAX=10
             KMAX=3
             NA=2*(NMAX**2)
             NB=2*NMAX*KMAX
             NW=2*NMAX
             NR=NMAX
             LA=1
             LAR=LA+NA
             LB=LAR+NA
             LBR=LB+NB
             LW=LBR+NB
             LR=LW+NW
             LAST=LR+NR
             IF(LAST-1.GT.LWORK) GOTO 900
             CALL F010CH(NREP,NMAX,KMAX,W(LA),W(LAR),W(LB),W(LBR),W(LW),
     +                                                       W(LR),OKPK)
             GOTO 90
   05        NMAX=10
             KMAX=3
             NA=2*NMAX**2
             NABAND=2*NMAX*(NMAX+1)
             NB=2*NMAX*KMAX
             LA=1
             LABAND=LA+NA
             LB=LABAND+NABAND
             LBR=LB+NB
             LAST=LBR+NB
             IF(LAST-1.GT.LWORK)  GO  TO  900
             CALL F406CH(NREP,NMAX,KMAX,W(LA),W(LABAND),W(LB),W(LBR),
     +                                                          OKPK)
             GOTO 90
  90         IF(.NOT. OKPK)  WRITE(*,1012) NAMEPK
             IF(      OKPK)  WRITE(*,1013) NAMEPK
             OK  =  OK .AND. OKPK
             CALL TIMEX( PKT(IPK) )
 200         CONTINUE
          IF(NPK .LT. 1)  OK  =  .FALSE.
          RETURN
#if defined(CERNLIB_NUMTIME)
 800      OK  =  .FALSE.
          WRITE(*,1020)
          RETURN
#endif
 900      OK  =  .FALSE.
          WRITE(*,1021)
          RETURN
1000      FORMAT(// 10H CHECK OF , A4,1H.)
1012      FORMAT(/ 5X, 24H ????? CHECK OF PACKAGE ,A4,
     +           18H HAS FAILED. ????? )
1013      FORMAT( 15X, 18H CHECK OF PACKAGE ,A4,12H SUCCESSFUL. )
1020      FORMAT(// 41H ????? TIMING HAS INSUFFICIENT WORK SPACE )
1021      FORMAT(// 42H ????? CHECK 2 HAS INSUFFICIENT WORK SPACE )
          END
