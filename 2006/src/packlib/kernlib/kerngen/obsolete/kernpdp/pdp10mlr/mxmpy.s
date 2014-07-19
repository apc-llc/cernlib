*
* $Id: mxmpy.s,v 1.1.1.1 1996/02/15 17:53:43 mclareni Exp $
*
* $Log: mxmpy.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:43  mclareni
* Kernlib
*
*
        TITLE   MXMPY   (A,B,C,I,J,K)
;#
; CERN PROGLIB# F110    MXMPY           .VERSION KERNPDP  1.00  750903
;#
;               SUPER FAST MATRIX MULTIPLY  -- TONY LAUCK
;ORIGINAL C.E.R.N. FORTRAN SOURCE CODE
;     SUBROUTINE MXMPY(A,B,C,I,J,K)
;     DIMENSION A(2),B(2),C(2)
;     IF((I.EQ.0).OR.(K.EQ.0)) GO TO 100
;  13 IA=1
;     IC=1
;     DO 25 L=1,I
;     IB =1
;     DO 23 M=1,K
;  15 C(IC)=0.
;  18 IF (J.EQ.0) GO TO 23
;     JA=IA
;     JB=IB
;     DO 20 N=1,J
;     C(IC)=C(IC)+A(JA)*B(JB)
;     JA=JA+1
;  20 JB=JB+K
;     IB=IB+1
;  23 IC=IC+1
;  25 IA=IA+J
; 100 RETURN
;     END
;ACCUMULATORS
LOOP2=0
J=1
IA=2
IB=3
JA=4
JB=5
Q1=6
I=7
K=10
Q2=11
Q3=12
T=13
T1=14
N=15
IC=16
P=17            ;NOT USED IN CASE OF OVERFLOW
        SEARCH HELP
#include "reentr.inc"
        HELLO( MXMPY)
        SKIPE   I,@3(16)        ;GET I,J,K
        SKIPN   K,@5(16)
        GOODBY
        SKIPN   J,@4(16)        ;DO MULTIPLY IF J NOT 0
        JRST    ZEROJ           ;ELSE ZERO OUT C
        MOVEM   15,SAV15
        MOVEM   17,SAV17
        MOVEM   16,SAV16        ;SAVE AC 16 AND START MULTIPLY
        MOVNM   K,NEGK          ;SET UP -K FOR AOBJN
        MOVEM   I,LCTR          ;SET UP OUTER LOOP COUNTER
;THIS CODE SETS UP THE INNER LOOP AC CODE
        MOVSI   LOOP2,  (MOVEI T1, )    ;START OF MIDDLE LOOP
        HRLI    J,      (MOVEI N, )
        MOVSI   IA,     (HRRI JA, )
        MOVSI   IB,     (HRRI JB, )
        MOVSI   JA,     (MOVE T,  )     ;BEGIN INNER LOOP
        MOVSI   JB,     (FMPR T,  )
        MOVE    Q1,     [FADR T1,T]
        HRLI    I,      (AOJ  JA, )
        HRLI    K,      (ADDI JB, )
        MOVE    Q2,     [SOJG N,JA]     ;END INNER LOOP
        MOVE    Q3,     [AOJA IB,ENDL2] ;GO FINISH MIDDLE LOOP
;NOW SET UP ADDRESS OF A,B,C
        HRRI    IA,@(16)
        MOVEI   T ,@1(16)
        MOVEM   T,BADR
        MOVEI   IC,@2(16)
        JRST BEGL3              ;GO START THE ACTION
ENDL2:  MOVEM   T1,(IC)         ;HERE TO END MIDDLE LOOP
        AOBJN   IC,LOOP2
ENDL3:  ADDI    IA,(J)          ;HERE TO END OUTER LOOP
        SOSG    LCTR
        JRST    DONE
BEGL3:  HRR     IB,BADR         ;HERE TO START OUTER LOOP
        HRL     IC,NEGK
        JRST    LOOP2
DONE:   MOVE    16,SAV16        ;RESTORE AC 16 AND RETURN
        MOVE    15,SAV15
        MOVE    17,SAV17
        GOODBY
ZEROJ:  IMUL    I,K             ;HERE TO ZERO ARRAY C IF J=0
        HRRZI   T,@2(16)
        ADDI    I,-1(T)         ;LAST WORD ADDRESS FOR BLT
        SETZM   (T)             ;ZERO FIRST WORD
        HRL     T,T             ;MAKE BLT POINTER
        ADDI    T,1
        BLT     T,(I)           ;CLEAR ALL OF C
        GOODBY
#include "reent1.inc"
SAV15:  0       ;AC 15 SAVED HERE
SAV17:  0
SAV16:  0       ;AC 16 SAVED HERE
NEGK:   0       ;-K SAVED HERE
LCTR:   0       ;OUTER LOOP COUNTER
BADR:   0       ;ADDRESS OF B
        PRGEND
#if defined(CERNLIB_MXFLAG1)
#ifndef CERNLIB_MXFLAG2
#define CERNLIB_MXFLAG2
#endif
#endif
