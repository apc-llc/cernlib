*
* $Id: iulook.s,v 1.1.1.1 1996/02/15 17:53:36 mclareni Exp $
*
* $Log: iulook.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:36  mclareni
* Kernlib
*
*
        TITLE   IULOOK
;#
; CERN PROGLIB# M410    IULOOK          .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (IULOOK)
       HRRZI      ,ACSAVE               ;SAVE AC'S
       BLT        ,ACSAVE+LOOPX
       MOVE    A01,@2(A16)              ;JL
       MOVE    A02,@3(A16)              ;JR
       MOVE    A03,@0(A16)              ;NCHAR
       CAILE   A03,5                    ;NCHAR MUST BE .LE. 5
       MOVEI   A03,5
       MOVEI      ,@1(A16)              ;ADR (IC)
       SOS
       HRRM       ,LOOPB+1
       MOVE    A04,[POINT 7,0,]         ;POINTS TO IULOOK
       MOVE       ,[XWD LOOPB+1,LOOP]   ;MOVE LOOP DOWN INTO FAST AC'S
       BLT        ,LOOPX
       MOVE       ,BLANK                ;PRESET IULOOK WITH BLANK
LOOPB: JRST       ,LOOP
       PHASE      6
LOOP:  MOVE    A05,(A01)                ;THIS LOOP CONTRACTS A MAXIMUM
       CAMLE   A01,A02                  ;OF 5 CHARACTERS IN A1-FORMAT
       JRST       ,END                  ;INTO 1 WORD (A5-FORMAT) .
       TLNN    A05,574000
       AOJA    A01,LOOP
       JUMPLE  A03,END
       ROT     A05,7
       IDPB    A05,A04
       AOS        ,A01
LOOPX: SOJA    A03,LOOP
       DEPHASE
END:   MOVE  LOOPX,[XWD ACSAVE+1,1]     ;RESTORE AC'S, EXIT
       BLT   LOOPX,LOOPX
        GOODBY
BLANK: ASCII   /     /
ACSAVE:BLOCK   LOOPX+1
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A05= 5
       A16=16
       PRGEND
#ifdef CERNLIB_TCGEN_IULOOK
#undef CERNLIB_TCGEN_IULOOK
#endif
