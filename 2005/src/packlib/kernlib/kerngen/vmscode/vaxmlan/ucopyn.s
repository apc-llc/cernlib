;
; $Id: ucopyn.s,v 1.1.1.1 1996/02/15 17:50:31 mclareni Exp $
;
; $Log: ucopyn.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:31  mclareni
; Kernlib
;
;
 .TITLE UCOPYN
;++
; CERN PROGLIB# V301    UCOPYN          .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 18/5/83
;
; SUBROUTINE UCOPYN(IA, IX, N)
; IX(I) =  - IA(I) , I=1,...,N
;
; Register usage:
; R0, R1        addresses of IA(I) and IX(I), resp.
; R2            counter from N to 1
;
; Note: For REAL*4 arrays use VCOPYN of F121.
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  UCOPYN,^M<R2>

        MOVL    @12(AP),R2              ; get counter
        BLEQ    20$                     ; return if <= 0
        MOVAL   @4(AP),R0               ; get base addresses of IA and IX
        MOVAL   @8(AP),R1

10$:    MNEGL   (R0)+,(R1)+             ; copy the negative value
        SOBGTR  R2,10$                  ; and loop
20$:    RET

        .END
