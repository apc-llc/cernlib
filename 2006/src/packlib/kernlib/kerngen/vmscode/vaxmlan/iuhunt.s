;
; $Id: iuhunt.s,v 1.1.1.1 1996/02/15 17:50:27 mclareni Exp $
;
; $Log: iuhunt.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:27  mclareni
; Kernlib
;
;
 .TITLE IUHUNT
;++
; CERN PROGLIB# V304    IUHUNT          .VERSION KERNVAX  2.14  850612
; ORIG.  J. VORBRUEGGEN 13/6/83
; CORRECTION FROM M.DERMOTT, RAL  12/6/85
;
; FUNCTION IUHUNT(IT, IVEC, N, INC)
; Examine the contents of IVEC at index 1, 1+INC, 1+2*INC,
; etc., up to N, for equality with IT. If a match is found,
; return the index; otherwise, return a value of 0.
;
; Register usage:
; R0    loop index
; R1    = IT
; R2    base address of IVEC
; R3    = N
; R4    = INC
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  IUHUNT,^M<R2,R3,R4>

        MOVL    @12(AP),R3              ; get N
        BLEQ    20$                     ; return if <= 0
        MOVL    #1,R0                   ; preset index
        MOVL    @16(AP),R4              ; get INC
        BEQL    40$                     ; if INC = 0, check IVEC(1) only
        MOVL    @4(AP),R1               ; get IT
        MOVAL   @8(AP),R2               ; get base address of IVEC
        SUBL2   #4,R2                   ; correct for index starting with 1

10$:    CMPL    (R2)[R0],R1             ; compare a longword
        BEQL    30$                     ; if equal, R0 already contains index
        ACBL    R3,R4,R0,10$            ; no, try again
                                        ; fall through if no match found
20$:    CLRL    R0                      ; return a value of zero
30$:    RET

40$:    CMPL    @4(AP),@8(AP)           ; check IVEC(1)
        BNEQ    20$                     ; if equal, R0 already equals 1
        RET

        .END
