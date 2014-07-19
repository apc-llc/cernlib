;
; $Id: iucomp.s,v 1.1.1.1 1996/02/15 17:50:27 mclareni Exp $
;
; $Log: iucomp.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:27  mclareni
; Kernlib
;
;
 .TITLE IUCOMP
;++
; CERN PROGLIB# V304    IUCOMP          .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 12/6/83
;
; INTEGER FUNCTION IUCOMP(IT, IVEC, N)
; Return index of first word in array IVEC of length N
; which is equal to IT; if none found or N = 0,
; a value of 0 is returned.
;
; Register usage:
; R0    = IT
; R1    address of IVEC(I)
; R2    counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  IUCOMP,^M<R2>

        MOVL    @12(AP),R2              ; get N
        BLEQ    20$                     ; return if <= 0
        MOVL    @4(AP),R0               ; get IT
        MOVAL   @8(AP),R1               ; get base address of IVEC

10$:    CMPL    R0,(R1)+                ; compare one longword
        BEQL    30$                     ; is it equal to IT ?
        SOBGTR  R2,10$                  ; no, try again
                                        ; fall through if no match found
20$:    CLRL    R0                      ; return a value of zero
        RET

30$:    SUBL3   R2,@12(AP),R0           ; compute index
        INCL    R0                      ; first element is IUCOMP = 1
        RET

        .END
