;
; $Id: uswop.s,v 1.1.1.1 1996/02/15 17:50:31 mclareni Exp $
;
; $Log: uswop.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:31  mclareni
; Kernlib
;
;
 .TITLE USWOP
;++
; CERN PROGLIB# V303    USWOP           .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 27/5/83
;
; SUBROUTINE USWOP(A, B, N)
; Exchange the vectors A and B, each of length N.
;
; Register usage:
; R0, R1        addresses of A(I) and B(I), resp.
; R2            buffer for A(I)
; R3            counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  USWOP,^M<R2,R3>

        MOVL    @12(AP),R3              ; get counter
        BLEQ    20$                     ; return if <= 0
        MOVAL   @4(AP),R0               ; get base address of A
        MOVAL   @8(AP),R1               ; and B
10$:    MOVL    (R0),R2                 ; buffer A(I)
        MOVL    (R1),(R0)+              ; copy B(I) to A(I)
        MOVL    R2,(R1)+                ; copy buffer to B(I)
        SOBGTR  R3,10$                  ; and loop
20$:    RET

        .END
