;
; $Id: ublow.s,v 1.1.1.1 1996/02/15 17:50:30 mclareni Exp $
;
; $Log: ublow.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:30  mclareni
; Kernlib
;
;
 .TITLE UBLOW
;++
; CERN PROGLIB# M409    UBLOW           .VERSION KERNVAX  1.04  800721
; ORIG.  DAVID MYERS, CERN, SEP 79
; MOD.   J. VORBRUEGGEN 19/5/83
;
; SUBROUTINE UBLOW(AVM, XVM, N)
; Unpack the N bytes at AVM to 1H (blank padded) strings at XVM
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  UBLOW,^M<R2>

        MOVL    @12(AP),R0              ; GET N
        BLEQ    99$                     ; IF .LE. 0 THEN RETURN
        MOVAL   @4(AP),R1               ; GET ADDRESS OF AVM
        MOVAL   @8(AP),R2               ; GET ADDRESS OF XVM

24$:    MOVL    #^A/    /,(R2)+         ; FIRST CLEAR WORD
        MOVB    (R1)+,-4(R2)            ; AND PUT CHAR IN FIRST BYTE
        SOBGTR  R0,24$                  ; NEXT CHAR
99$:    RET
        .END
