;
; $Id: uh1toc.s,v 1.1.1.1 1996/02/15 17:50:31 mclareni Exp $
;
; $Log: uh1toc.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:31  mclareni
; Kernlib
;
;
 .TITLE UH1TOC
;++
; CERN PROGLIB# M409    UH1TOC          .VERSION KERNVAX  1.13  820606
; ORIG. 4/6/82, JZ, CERN
; MOD.  J.VORBRUEGGEN   31/5/83
;
; SUBROUTINE UH1TOC(AVM, XVC, N)
; Convert the data in 1H format in AVM to XVC, checking whether
; XVC is passed by descriptor (real CHARACTER data) or by
; reference (anything else).
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  UH1TOC,^M<R2,R3>

        MOVL    @12(AP),R0              ; R0 = N
        BLEQ    99$                     ; --> EXIT IF N .LE. 0
        MOVAL   @4(AP),R3               ; R3 = ADR(AVM)
        MOVAL   @8(AP),R2               ; R2 = ADR(D FOR XVC) OR ADR(XVC)
        CMPB    #14,2(R2)               ; CHECK BYTE 3 IN DESCR IS =14
        BNEQ    24$
        CMPB    #8,3(R2)                ; CHECK BYTE 4 IN DESCR IS <8
        BLEQ    24$
        MOVAL   @4(R2),R2               ; R2 = ADR(XVC) FROM DESCIPTOR

24$:    MOVB    (R3),(R2)+              ; GET CHAR AND BUNCH IT
        ADDL2   #4,R3                   ; SKIP TO NEXT WORD
        SOBGTR  R0,24$                  ; NEXT CHAR
99$:    RET
        .END
