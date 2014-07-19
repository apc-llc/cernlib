;
; $Id: vasum.s,v 1.1.1.1 1996/02/15 17:50:32 mclareni Exp $
;
; $Log: vasum.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:32  mclareni
; Kernlib
;
;
 .TITLE VASUM
;++
; CERN PROGLIB# F121    VASUM           .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 4/5/83
;
; FUNCTION VASUM(A, N)
; Give sum of absolute values of A(I), I=1,...,N
;
; Register usage:
; R0    sum
; R1    absolute value of A(I)
; R2    address of A(I)
; R3    counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VASUM,^M<R2,R3>

        CLRF    R0                      ; initialize sum
        MOVL    @8(AP),R3               ; get counter
        BLEQ    20$                     ; return if <= 0
        MOVAL   @4(AP),R2               ; get base address of A
10$:    BICL3   #^X8000,(R2)+,R1        ; get absolute value of A(I)
        ADDF2   R1,R0                   ; add it
        SOBGTR  R3,10$                  ; and loop
20$:    RET

        .END
