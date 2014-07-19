*
* $Id: amaxmu.s,v 1.1.1.1 1996/02/15 17:53:34 mclareni Exp $
*
* $Log: amaxmu.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:34  mclareni
* Kernlib
*
*
        TITLE   AMAXMU
;#
; CERN PROGLIB# E103    AMAXMU          .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (AMAXMU)
       HRRZI      ,ACSAVE               ;SAVE AC'S
       BLT        ,ACSAVE+LOOPX
       MOVEI      ,@0(A16)              ;ADR(A)
       HRRM       ,LOOPB+2
       SKIPG   A01,@1(A16)              ;LOAD IDO
       JRST       ,LOOPE
       SKIPG   A02,@2(A16)              ;LOAD IW
       JRST       ,LOOPE
       HRRM    A02,LOOPB+10
       MOVE       ,@3(A16)              ;INSERT NA
       HRRM       ,LOOPB+7
       MOVE       ,[XWD LOOPB+1,LOOP]   ;MOVE LOOP DOWN INTO FAST AC'S
       BLT        ,LOOPX
       SETZB      ,A05                  ;AM=0.  II=1
LOOPB: JRST       ,LOOP
       PHASE      6
LOOP:  MOVEM   A05,A04                  ;   DO 19 JN=1,IDO
LOOP1: MOVM    A03,(A04)
       CAMG       ,A03                  ;   DO 17 JK=1,IW
       MOVE       ,A03                  ;   AM = AMAX1(AM,ABS(A(I)))
       AOS        ,A04                  ;17 I = I+1
       SOJG    A02,LOOP1
       ADDI    A05,
       MOVEI   A02,                     ;19 II = II+NA
       SOJG    A01,LOOP
LOOPX: JRST       ,LOOPE
       DEPHASE
LOOPE: MOVE  LOOPX,[XWD ACSAVE+1,1]     ;RESTORE AC'S, EXIT
       BLT   LOOPX,LOOPX
        GOODBY
ACSAVE:BLOCK   LOOPX+1
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A05= 5
       A16=16
        PRGEND
#ifdef CERNLIB_TCGEN_AMAXMU
#undef CERNLIB_TCGEN_AMAXMU
#endif
