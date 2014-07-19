;
; $Id: ufill.s,v 1.1.1.1 1996/02/15 17:50:31 mclareni Exp $
;
; $Log: ufill.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:31  mclareni
; Kernlib
;
;
 .TITLE UFILL
;++
; CERN PROGLIB# V300    UFILL           .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 30/4/83
;
; SUBROUTINE UFILL(A, JL, JN, STUFF)
; Preset array from element JL to JN with STUFF
;
; Register usage:
; R0    address of current array element
; R1    = STUFF
; R2    counter from JN-JL to zero
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  UFILL,^M<R2>

        MOVAL   @4(AP),R0               ; get base address of A
        MOVL    @8(AP),R1               ; get JL
        MOVAL   -4(R0)[R1],R0           ; compute address of A(JL)
        SUBL3   R1,@12(AP),R2           ; count of longwords to be filled
        MOVL    @16(AP),R1              ; get STUFF

10$:    MOVL    R1,(R0)+                ; fill
        SOBGEQ  R2,10$                  ; and loop
        RET

        .END
