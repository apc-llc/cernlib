 .TITLE EPIO_MACRO
;
;       MAGTAPE ROUTINES FOR EPIO I/O PACKAGE
;
;       F.C.    VERSION 3.3    24-2-87  CHECK FOR INVALID BUFFER SIZE
;       F.C.    VERSION 3.2     2-6-86  CHANGE L^LIST(R1) --> LIST[R1]
;       A.F.R.  VERSION 3.1     2-6-82  ADD 2 WORDS IN EPCOMM
;       A.F.R.  VERSION 3       9-9-81  ADD EPMTEOF
;       A.R.R.  VERSION 2       1-6-81  CHANGE TO QIO READ
;       A.F.R.  VERSION 1.1     4-5-81  FIX LIST(R1) --> L^LIST(R1)
;       A.F.R.  VERSION 1       20-3-81
;
;
;       CALL EPOPENS_VAX
;       CALL EPMTREAD(BUF,MAXLEN,NB,IERR)
;       CALL EPMTREW(IERR)
;       CALL EPMTEOF(IERR)
;       CALL EPMTWRITE(BUF,NB,IERR)
;
IOSB:   .BLKW   4
FIB_D:
        .LONG   ENDFIB-FIB
        .ADDRESS FIB
FIB:    .LONG   0
        .WORD   0,0,0,0,0,0
        .LONG   0
        .WORD   0,0
ENDFIB:
        .PSECT  EPCOMM,PIC,REL,OVR,NOEXE,GBL,WRT,SHR,LONG
;                               THIS IS THE EPCOMM COMMON BLOCK
NMUNIT: .BLKL   1
NWUNIT: .BLKL   1
NCONT:  .BLKL   1
ISTART: .BLKL   1
LASTUT: .BLKL   1
LREF:   .BLKL   1
LIST:
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        $FABDEF
        $IODEF
ICHAN  = 24                           ;DISPLACEMENT TO 25TH WORD OF UNIT GROUP
MODE   = 15                           ;DISPLACEMENT TO 16TH WORD
        .ENTRY  EPOPENS_VAX,^M<R2,R3>
;
;       CALL EPOPENS_VAX IS A USER_OPEN ROUTINE THAT
;       REMEMBERS THE CHANNEL NUMBER  AT LIST(ISTART+25)
;
;
        MOVL    4(AP),R3                       ; MOVE FAB ADDRESS TO R3
        $FAB_STORE      FAB=R3,FOP=UFO
        $OPEN   FAB=R3
        BLBC    R0,105$                        ; CHECK ERROR
        MOVL    FAB$L_STV(R3),R2               ; GET CHANNEL NUMBER
        $QIOW_S CHAN=R2,FUNC=#IO$_ACCESS!IO$M_ACCESS,-
                IOSB = IOSB,P1=FIB_D           ; RETURNS 64 FOR FOREIGN TAPE
        CMPL    #SS$_DEVFOREIGN,R0
        BEQL    110$
        RET
110$:
        MOVL    #1,R0                          ; PUT IN RETURN CODE SUCCESS
        MOVL    ISTART,R1                      ; ADDRESS OF ARRAY ISTART OF
                                               ; EPIOCOM
        ADDL2   #<ICHAN>,R1                    ; DISPLACEMENT OF CHANNEL
                                               ; LOCATION IN LIS
        MOVL    R2,LIST[R1]                    ; STORE CHANNEL NUMBER
105$:
        RET
;
        .ENTRY EPMTREAD,^M<R2>
;
;       CALL EPMTREAD(BUF,MAXLEN,NB,IERR) GETS THE NEXT RECORD FROM TAPE
;       BUF(MAXLEN) IS THE BYTE ARRAY NB IS THE NUMBER OF BYTES READ
;       IERR IS THE ERROR CODE
;
        MOVL    ISTART,R1                      ; ADDRESS OF ARRAY ISTART OF
                                               ; EPIOCOM
        ADDL2   #<ICHAN>,R1                    ; CHANNEL NUMBER FOR THIS UNIT
        MULW2   #2,R1                          ; GO TO WORDS
        MOVL    @8(AP),R2                      ; THIS IS THE BUFFER SIZE
        CMPL    R2,#^XFFFE                     ; CHECK AGAINST THE MAX
        BLEQU   110$                           ; OK, BRANCH
        MOVZWL  #^XFFFE,R2                     ; SET IT TO THE MAX
110$:   $QIOW_S CHAN=LIST[R1],FUNC=#IO$_READVBLK,-
                P1=@4(AP),P2=R2,IOSB=IOSB      ; READ THE NEXT BLOCK
        BLBC    R0,120$                        ; THIS SHOULD NOT HAPPEN
        MOVW    IOSB,@16(AP)                   ; THIS IS THE ERROR CODE
        MOVW    IOSB+2,@12(AP)                 ; THIS IS THE BYTE COUNT
        RET
120$:
        MOVW    R0,@16(AP)                     ; HERE IS THE BAD ERROR
        CLRW    @12(AP)                        ; BYTE COUNT IS 0
        RET
        .ENTRY EPMTWRITE,^M<>
;
;       CALL EPMTWRITE(BUF,NB,IERR) WRITES THE NEXT RECORD
;       STARTING AT BUF(1) FOR NB BYTES AND RETURNING ERROR IERR
;
        MOVL    ISTART,R1                      ; ADDRESS OF ARRAY ISTART OF
                                               ; EPIOCOM
        ADDL2   #<ICHAN>,R1                    ; CHANNEL NUMBER FOR THIS UNIT
        MULW2   #2,R1                          ; GO TO WORDS
        $QIOW_S CHAN=LIST[R1],FUNC=#IO$_WRITEVBLK,-
                P1=@4(AP),P2=@8(AP),IOSB=IOSB  ; READ THE NEXT BLOCK
        BLBC    R0,125$                        ; THIS SHOULD NOT HAPPEN
        MOVW    IOSB,@12(AP)                   ; THIS IS THE ERROR CODE
        RET
125$:
        MOVW    R0,@12(AP)                     ; HERE IS THE BAD ERROR
        RET
        .ENTRY EPMTREW,^M<R2,R3>
;
;       CALL EPMTREW(IERR) ADDS DOUBLE EOF IF WRITE AND THEN REWINDS THE UNIT
;
        MOVL    #1,@4(AP)                      ; SUCCESS RETURN
        MOVL    ISTART,R3
        ADDL2   #<ICHAN>,R3
        MULW2   #2,R3                          ; GO TO WORDS
        MOVL    ISTART,R2
        ADDL2   #<MODE>,R2
        CMPL    #1,LIST[R2]                    ; MODE IS 1 FOR WRITE
        BNEQU   130$
        $QIOW_S CHAN=LIST[R3],FUNC=#IO$_WRITEOF,IOSB=IOSB    ;WRITE EOF
        BLBS    IOSB,127$                      ; SKIP IF NO TROUBLE
        MOVW    IOSB,@4(AP)                    ; RETURN CODE IF TROUBLE
127$:
        $QIOW_S CHAN=LIST[R3],FUNC=#IO$_WRITEOF,IOSB=IOSB    ;WRITE EOF
130$:
        $QIOW_S CHAN=LIST[R3],FUNC=#IO$_REWIND
        RET
        .ENTRY EPMTEOF,^M<R2,R3>
;
;       CALL EPMTEOF(IERR) ADDS DOUBLE EOF AND BACKSPACES OVER LAST ONE
;
        MOVL    #1,@4(AP)                      ; SUCCESS RETURN
        MOVL    ISTART,R3
        ADDL2   #<ICHAN>,R3
        MULW2   #2,R3                          ; GO TO WORDS
        MOVL    ISTART,R2
        ADDL2   #<MODE>,R2
        CMPL    #1,LIST[R2]                    ; MODE IS 1 FOR WRITE
        BNEQU   230$
        $QIOW_S CHAN=LIST[R3],FUNC=#IO$_WRITEOF,IOSB=IOSB    ;WRITE EOF
        BLBS    IOSB,227$                      ; SKIP IF NO TROUBLE
        MOVW    IOSB,@4(AP)                    ; RETURN CODE IF TROUBLE
227$:
        $QIOW_S CHAN=LIST[R3],FUNC=#IO$_WRITEOF,IOSB=IOSB    ;WRITE EOF
230$:
        $QIOW_S CHAN=LIST[R3],FUNC=#IO$_SKIPRECORD,IOSB=IOSB,P1=-1
        RET
        .END
