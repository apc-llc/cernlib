;
; $Id: ubunch.s,v 1.1.1.1 1996/02/15 17:50:30 mclareni Exp $
;
; $Log: ubunch.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:30  mclareni
; Kernlib
;
;
 .TITLE UBUNCH
;++
; CERN PROGLIB# M409    UBUNCH          .VERSION KERNVAX  1.04  800721
; ORIG.  DAVID MYERS, CERN, SEP 79
; MOD.  J. VORBRUEGGEN  31/5/83
;
; SUBROUTINE UBUNCH(AVM, XVM, N)
; Copy the N bytes at AVM in 1H format to the XVM
; in 4H format.
; If XVM starts on longword boundary then remaining
; bytes in last word are blank filled.
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  UBUNCH,^M<R2,R3>

        MOVL    @12(AP),R0              ;GET N
        BLEQ    99$                     ;IF .LE. 0 THEN RETURN
        MOVAL   @4(AP),R3               ;GET ADDR. OF AVM
        MOVAL   @8(AP),R2               ;GET ADDR. OF XVM
24$:    MOVB    (R3),(R2)+              ;GET CHAR AND BUNCH IT
        ADDL2   #4,R3                   ;SKIP TO NEXT WORD
        SOBGTR  R0,24$                  ;NEXT CHAR
        MOVL    R2,R1                   ;SAVE ADDRESS
        BLBC    R1,76$                  ;SKIP IF N EVEN
        MOVB    #^A/ /,(R2)+            ;OTHERWISE FILL ONE BLANK
        BBS     #1,R1,99$               ;SKIP IF N WAS ODD-EVEN
        BRB     79$

76$:    BBC     #1,R1,99$               ;SKIP IF N WAS EVEN-EVEN

79$:    MOVW    #^A/  /,(R2)            ;OTHERWISE FILL TWO BLANKS

99$:    RET
        .END
