;
; $Id: vsum.s,v 1.1.1.1 1996/02/15 17:50:33 mclareni Exp $
;
; $Log: vsum.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:33  mclareni
; Kernlib
;
;
 .TITLE VSUM
;++
; CERN PROGLIB# F121    VSUM            .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 4/5/83
;
; FUNCTION VSUM(A, N)
; Give sum of A(I), I=1,...,N
;
; Register usage:
; R0    sum
; R1    address of A(I)
; R2    counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VSUM,^M<R2>

        CLRF    R0                      ; initialize sum
        MOVL    @8(AP),R2               ; get counter
        BLEQ    20$                     ; return if <= 0
        MOVAL   @4(AP),R1               ; get base address of A

10$:    ADDF2   (R1)+,R0                ; sum
        SOBGTR  R2,10$                  ; and loop
20$:    RET

        .END
