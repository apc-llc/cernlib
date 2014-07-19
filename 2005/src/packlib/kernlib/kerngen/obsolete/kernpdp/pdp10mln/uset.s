*
* $Id: uset.s,v 1.1.1.1 1996/02/15 17:53:38 mclareni Exp $
*
* $Log: uset.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:38  mclareni
* Kernlib
*
*
        TITLE   USET
;#
; CERN PROGLIB# M410    USET            .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
.COMMON SLATE [50]
        SEARCH HELP
        HELLO (USET)
       HRRZI      ,ACSAVE               ;SAVE AC'S
       BLT        ,ACSAVE+LOOPX
       MOVEI   A01,@1(A16)              ;ADR(IC)
       MOVE    A02,@2(A16)              ;JL
       MOVE    A03,@3(A16)              ;JR
       MOVE    A04,@0(A16)              ;INT
       SOS        ,A01
       MOVE       ,[XWD LOOPB+1,LOOP]   ;MOVE LOOP DOWN INTO
       BLT        ,LOOPX                ;FAST ACCUMULATORS
       HRRM    A01,LOOP+4               ;INSERT ADDRESSES
       MOVEI   A01,SLATE
       SETZM      ,SLATE
LOOPB: JRST       ,LOOP
       PHASE      6
LOOP:  CAMGE   A03,A02                  ;THIS LOOP CONVERTS THE DIGITS
       JRST       ,END                  ;OF AN UNSIGNED INTEGER TO LEFT
       IDIVI   A04,12                   ;JUSTIFIED ASCII CHARACTERS .
       MOVE       ,TABLE(A05)
       MOVEM      ,(A03)
       AOS        ,SLATE
       CAIL    A04,1
       SOJA    A03,LOOP
LOOPX: SOJA    A03,END
       DEPHASE
       PAGE
END:   MOVEM   A03,1(A01)               ;SET JEND FOR IUEND
       HRLZI      ,ACSAVE               ;RESTORE AC'S , EXIT
       BLT        ,LOOPX
        GOODBY
TABLE: ASCII   /0    /
       ASCII   /1    /
       ASCII   /2    /
       ASCII   /3    /
       ASCII   /4    /
       ASCII   /5    /
       ASCII   /6    /
       ASCII   /7    /
       ASCII   /8    /
       ASCII   /9    /
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A05= 5
       A16=16
ACSAVE:BLOCK   LOOPX+1
       PRGEND
#ifdef CERNLIB_TCGEN_USET
#undef CERNLIB_TCGEN_USET
#endif
