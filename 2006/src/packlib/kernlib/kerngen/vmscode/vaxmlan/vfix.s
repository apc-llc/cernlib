;
; $Id: vfix.s,v 1.1.1.1 1996/02/15 17:50:32 mclareni Exp $
;
; $Log: vfix.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:32  mclareni
; Kernlib
;
;
 .TITLE VFIX
;++
; CERN PROGLIB# F121    VFIX            .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 2/5/83
;
; SUBROUTINE VFIX(A, IX, N)
; IX(I) = IFIX(A(I)) , I=1,...,N
;
; Register usage:
; R0, R1        addresses of A(I) and IX(I), resp.
; R2            counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VFIX,^M<R2>

        MOVL    @12(AP),R2              ; get counter
        BLEQ    20$                     ; return if <= 0
        MOVAL   @4(AP),R0               ; get base addresses of A and IX
        MOVAL   @8(AP),R1

10$:    CVTFL   (R0)+,(R1)+             ; convert to integer
        SOBGTR  R2,10$                  ; and loop
20$:    RET

        .END
