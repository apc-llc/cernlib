*
* $Id: uleft.s,v 1.1.1.1 1996/02/15 17:53:38 mclareni Exp $
*
* $Log: uleft.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:38  mclareni
* Kernlib
*
*
        TITLE   ULEFT
;#
; CERN PROGLIB# M410    ULEFT           .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
.COMMON SLATE [50]
        SEARCH HELP
        HELLO (ULEFT)
        JSR     ,ARG
       MOVE       ,[XWD LOOPB+1,LOOP]   ;MOVE LOOP DOWN INTO
       BLT        ,LOOPX                ;FAST ACCUMULATORS
       MOVEM   A02,A04
       HRRM    A01,LOOP+2               ;INSERT ADDRESSES
       HRRM    A01,LOOP+5
       HRRM    A01,LOOP+6
       MOVE    A01,BLANK
LOOPB: JRST       ,LOOP                 ;GO ON
       PHASE      5
LOOP:  CAMLE   A02,A03                  ;THIS LOOP CONTRACTS THE VECTOR
       AOJA    A04,LOOPE                ;IC BY SUPPRESSING THE BLANKS
       MOVE       ,(A02)                ;AND SHIFTING DOWN
       CAMN       ,A01
       AOJA    A02,LOOP
       MOVEM   A01,(A02)
       MOVEM      ,(A04)
       AOS        ,A04
LOOPX: AOJA    A02,LOOP
       DEPHASE
LOOPE: MOVEI   A01,SLATE
       MOVEM   A04,1(A01)               ;SET JEND FOR IUEND
       SUB     A04,@1(A16)
       MOVEM   A04,0(A01)               ;SET ND FOR IUEND
       JRST       ,END
ARG:    0
       HRRZI      ,ACSAVE               ;SAVE AC'S
       BLT        ,ACSAVE+LOOPX
       MOVEI   A01,@0(A16)              ;ADR(IC)
       MOVE    A02,@1(A16)              ;JL
       MOVE    A03,@2(A16)              ;JR
       SOS        ,A02
       SOS        ,A03
        JRST    2,@ARG
        HELLO (URIGHT)
        JSR     ,ARG
       MOVE       ,[XWD LOOP1B+1,LOOP]  ;MOVE LOOP DOWN INTO
       BLT        ,LOOP1X               ;FAST ACCUMULATORS
       MOVEM   A03,A04
       HRRM    A01,LOOP1+2              ;INSERT ADDRESSES
       HRRM    A01,LOOP1+5
       HRRM    A01,LOOP1+6
       MOVE    A01,BLANK
LOOP1B:JRST       ,LOOP                 ;GO ON
       PHASE      5
LOOP1: CAMGE   A03,A02                  ;THIS LOOP CONTRACTS THE VECTOR
       AOJA    A04,LOOP1E               ;IC BY SUPRESSING THE BLANKS
       MOVE       ,(A03)                ;AND SHIFTING THE ELEMENTS UPWAR
       CAMN       ,A01
       SOJA    A03,LOOP1
       MOVEM   A01,(A03)
       MOVEM      ,(A04)
       SOS        ,A04
LOOP1X:SOJA    A03,LOOP1
       DEPHASE
LOOP1E:MOVEI   A01,SLATE
       MOVEM   A04,1(A01)               ;SET JEND FOR IUEND
       SUB     A04,@2(A16)
       MOVNM   A04,0(A01)               ;SET ND FOR IUEND
END:   HRLZI      ,ACSAVE               ;RESTORE AC'S
       BLT        ,LOOPX
        GOODBY
BLANK: ASCII   /     /
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A16=16
ACSAVE:BLOCK   LOOPX+1
       PRGEND
#ifdef CERNLIB_TCGEN_ULEFT
#undef CERNLIB_TCGEN_ULEFT
#endif
#ifdef CERNLIB_TCGEN_URIGHT
#undef CERNLIB_TCGEN_URIGHT
#endif
