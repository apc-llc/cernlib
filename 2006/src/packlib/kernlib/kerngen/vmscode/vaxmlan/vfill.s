;
; $Id: vfill.s,v 1.1.1.1 1996/02/15 17:50:31 mclareni Exp $
;
; $Log: vfill.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:31  mclareni
; Kernlib
;
;
 .TITLE VFILL
;++
; CERN PROGLIB# F121    VFILL           .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 30/4/83
;
; SUBROUTINE VFILL(X, N, ALPHA)
; Preset with ALPHA the array X(I) , I=1,...,N
;
; Register usage:
; R0    address of X(I)
; R1    = ALPHA
; R2    counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VFILL,^M<R2>

        MOVL    @8(AP),R1               ; get counter
        BLEQ    20$                     ; return if <= 0
        MOVAL   @4(AP),R0               ; get base address 0f X
        MOVL    @12(AP),R2              ; get ALPHA

10$:    MOVL    R2,(R0)+                ; fill
        SOBGTR  R1,10$                  ; and loop
20$:    RET

        .END
