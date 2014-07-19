*
* $Id: iuforw.s,v 1.1.1.1 1996/02/15 17:53:35 mclareni Exp $
*
* $Log: iuforw.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:35  mclareni
* Kernlib
*
*
        TITLE   IUFORW
;#
; CERN PROGLIB# M410    IUFORW          .VERSION KERNPDP  1.00  750903
;#
.COMMON SLATE [50]
        SEARCH HELP
        HELLO (IUFORW)
       SETOM      ,WAY
        JSR     ,ARG1
       JUMPG   A01,EXIT                 ;JL MUST BE .LE. JR
GOON:  MOVE       ,[XWD LOOPB+1,LOOP]
       BLT        ,LOOPX
       SETZM                            ;IUFORW = 0
       SETZM      ,DIGCNT               ;ND = 0
LOOPB: JRST       ,LOOP                 ;GO ON
       PHASE      3
LOOP:  MOVE    A02,(A01)                ;THIS LOOP CONVERTS CHARACTERS
       JUMPG   A01,END                  ;IN A1-FORMAT TO 1 WORD CON -
       TLNN    A02,574000               ;TAINING AN UNSIGNED INTEGER
       AOJA    A01,LOOP                 ;BLANKS ARE IGNORED
       LSH     A02,-35
       SUBI    A02,60
       JUMPL   A02,END
       CAILE   A02,11
       JRST       ,END
       IMULI      ,12
       ADD        ,A02
       AOS        ,DIGCNT
LOOPX: AOJA    A01,LOOP
       DEPHASE
EXIT:  SETZM      ,DIGCNT               ;ERROR EXIT
       SETZM
END:   MOVEI   A03,SLATE                ;NORMAL END
       MOVE    A16,ACSAVE+16            ;SET JEND AND ND FOR IUEND
       ADD     A01,@2(A16)
       MOVE    A02,STOPNT
       ADD     A02,@1(A16)
       SKIPN      ,WAY
       MOVEM   A02,A01
       MOVEM   A01,1(A03)
       MOVE    A01,DIGCNT
       MOVEM   A01,SLATE
       MOVE  LOOPX,[XWD ACSAVE+1,1]     ;RESTORE AC'S, EXIT
       BLT   LOOPX,LOOPX
        GOODBY
        HELLO (IUBACK)
       SETZM      ,WAY
        JSR     ,ARG1
       MOVNM   A01,STOPNT
       MOVNM   A01,ACSAVE
       JUMPG   A01,EXIT                 ;JL MUST BE .LE. JR
       ADD        ,A01
       HRRM       ,SEARCB+1             ;INSERT ADR (IC)
       MOVN    A01,A01
       MOVE       ,[XWD SEARCB+1,SEARCH]
       BLT        ,SEARCX               ;MOVE LOOP DOWN INTO FAST AC'S
SEARCB:JRST       ,SEARCH               ;GO ON
       PHASE      3
SEARCH:MOVE    A02,(A01)                ;THIS LOOP SEARCHES THE FIRST
       JUMPL   A01,SA                   ;ASCII-CODED INTEGER IN ARRAY IC
       TLNN    A02,574000               ;BLANKS ARE IGNORED
       SOJA    A01,SEARCH
       LSH     A02,-35
       SUBI    A02,60
       JUMPL   A02,SA
       CAIG    A02,11
       SOJA    A01,SEARCH
SA:    MOVEM   A01,STOPNT
       SUB     A01,ACSAVE
SEARCX:AOJA    A01,GOON
       DEPHASE
ARG1:   0
       HRRZI      ,ACSAVE               ;SAVE AC'S
       BLT        ,ACSAVE+LOOPX
       MOVEI      ,@0(A16)
       MOVE    A01,@2(A16)              ;INSERT ADR (IC)
       ADD        ,A01
       SOS
       HRRM       ,LOOPB+1
       SUB     A01,@1(A16)              ;CALCULATE WORD COUNT
       MOVN    A01,A01
        JRST    2,@ARG1
WAY:   0
STOPNT:0
DIGCNT:0
ACSAVE:BLOCK   LOOPX+1
       A01= 1
       A02= 2
       A03= 3
       A16=16
       PRGEND
#ifdef CERNLIB_TCGEN_IUBACK
#undef CERNLIB_TCGEN_IUBACK
#endif
#ifdef CERNLIB_TCGEN_IUFORW
#undef CERNLIB_TCGEN_IUFORW
#endif
