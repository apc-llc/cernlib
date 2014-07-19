;
; $Id: iufind.s,v 1.1.1.1 1996/02/15 17:50:27 mclareni Exp $
;
; $Log: iufind.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:27  mclareni
; Kernlib
;
;
 .TITLE IUFIND
;++
; CERN PROGLIB# V304    IUFIND          .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 13/6/83
;
; INTEGER FUNCTION IUFIND(IT, IVEC, JL, JR)
; Return index of first word in array IVEC in the
; range JL to JR which is equal to IT; if none found
; a value of 0 is returned.
;
; Register usage:
; R0    = IT
; R1    address of IVEC(I), I = JR, JL
; R2    counter from JR-JL to 0
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  IUFIND,^M<R2>

        MOVL    @4(AP),R0               ; get IT
        MOVAL   @8(AP),R1               ; get base address of IVEC
        MOVL    @12(AP),R2              ; get JL
        MOVAL   -4(R1)[R2],R1           ; compute address of IVEC(JL)
        SUBL3   R2,@16(AP),R2           ; compute counter

10$:    CMPL    R0,(R1)+                ; compare one longword
        BEQL    20$                     ; are they equal ?
        SOBGEQ  R2,10$                  ; no, try again
                                        ; fall through if no match found
        MOVL    @16(AP),R0              ; return a value of JL+1
        INCL    R0
        RET

20$:    SUBL3   R2,@16(AP),R0           ; compute correct index
        RET

        .END
