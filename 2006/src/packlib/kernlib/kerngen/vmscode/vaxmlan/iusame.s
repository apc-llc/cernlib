;
; $Id: iusame.s,v 1.1.1.1 1996/02/15 17:50:27 mclareni Exp $
;
; $Log: iusame.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:27  mclareni
; Kernlib
;
;
 .TITLE IUSAME
;++
; CERN PROGLIB# M501    IUSAME          .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 18/5/83
;
; INTEGER FUNCTION IUSAME(VECT, JL, JR, MIN, JSAME)
; The function searches for at least MIN consecutive
; occurences of the same longword in VECT(JL) to VECT(JR).
; If such a sequence is found, JSAME contains the index of the
; first longword of the sequence and the function value is the
; number of occurences. Otherwise, IUSAME=0 and JSAME=JR+1.
;
; Register usage:
; R0    address of VECT(I), I = JL, JR
; R1    = MIN
; R2    counter of occurences
; R3    index (R5) when current item first occured
; R4    = JR
; R5    counter from JL to JR
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  IUSAME,^M<R2,R3,R4,R5>

        MOVAL   @4(AP),R0               ; get base address of VECT
        MOVL    @8(AP),R5               ; get JL
        MOVAL   -4(R0)[R5],R0           ; compute address of VECT(JL)
        MOVL    @12(AP),R4              ; get JR
        MOVL    R5,R3                   ; preset index to JL
        CLRL    R2                      ; preset occurence counter
        MOVL    @16(AP),R1              ; get MIN
        BRB     20$                     ; start main loop

10$:    CMPL    (R0)+,(R0)              ; compare VECT(I) and VECT(I+1)
        BEQL    20$                     ; if equal, go on looking
        CMPL    R1,R2                   ; not equal - have we reached MIN ?
        BLEQU   30$
        CLRL    R2                      ; no - reset occurence counter
        MOVL    R5,R3                   ; and index
20$:    INCL    R2                      ; increment occurence counter
        AOBLEQ  R4,R5,10$               ; and loop

        CMPL    R1,R2                   ; finished with examining VECT
        BGTRU   40$                     ; have we reached MIN equalities ?
30$:    MOVL    R2,R0                   ; yes, return index as function value
        MOVL    R3,@20(AP)              ; and first occurence index in JSAME
        RET
                                        ; we didn't get MIN equal longwords
40$:    CLRL    R0                      ; so return a zero as function value
        MOVL    R5,@20(AP)              ; and JR+1 in JSAME
        RET

        .END
