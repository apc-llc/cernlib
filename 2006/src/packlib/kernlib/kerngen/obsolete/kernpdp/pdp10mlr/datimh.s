*
* $Id: datimh.s,v 1.1.1.1 1996/02/15 17:53:42 mclareni Exp $
*
* $Log: datimh.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:42  mclareni
* Kernlib
*
*
        TITLE DATIMH
;#
; CERN PROGLIB# Z007    DATIMH          .VERSION KERNPDP  1.02  761001
;#
        SEARCH HELP
#include "reentr.inc"
        T0==1
        T1=2
        T2==3
        T3==4
        T4==5
        T5==6
        T6==7
        T7=10
        HELLO(DATIMH)
        DATE    T0,
        IDIVI   T0,^D31
        MOVEI   T2,1(T1)                        ;DAY
        IDIVI   T0,^D12                         ;MONTH
        ADDI    T1,1
        ADDI    T0,^D64                         ;YEAR
        MOVEI   T6,3
        MOVEI   T4,57                           ;GET SLASH
        MOVE    T5,[POINT 7,0]                  ;SETUP POINTER
        ADDI    T5,@(16)
        PUSHJ   17,CONVER                       ;DO CONVERSION TO ASCII
        MSTIME  T2,                             ;GET TIME IN MILLI SECONDS
        IDIVI   T2,^D1000                       ;GET SECONDS
        IDIVI   T2,^D3600                       ;GET HOURS IN T2
        IDIVI   T3,^D60                         ;GET MINUTES
        MOVE    T1,T3
        MOVE    T0,T4                           ;GET SECONDS
        MOVEI   T6,3
        MOVEI   T4,56                           ;GET POINT
        MOVE    T5,[POINT 7,]                   ;SET UP POINTER
        ADDI    T5,@1(16)
        PUSHJ   17,CONVER                       ;DO CONVERSION
        GOODBY
CONVER: IDIVI   T2,12
        ADDI    T2,60
        ADDI    T3,60                           ;GET ASCII
        IDPB    T2,T5
        IDPB    T3,T5                           ;DEPOSIT
CONT:   SOJN    T6,AGAIN
        POPJ    17,
AGAIN:  IDPB    T4,T5                           ;NEXT WORD
        MOVE    T2,T1
        CAIE    T6,2
        MOVE    T2,T0
        JRST    CONVER
        PRGEND
#ifdef CERNLIB_TCGEN_DATIMH
#undef CERNLIB_TCGEN_DATIMH
#endif
