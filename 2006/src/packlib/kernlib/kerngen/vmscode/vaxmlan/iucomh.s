;
; $Id: iucomh.s,v 1.1.1.1 1996/02/15 17:50:27 mclareni Exp $
;
; $Log: iucomh.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:27  mclareni
; Kernlib
;
;
 .TITLE IUCOMH
;++
; CERN PROGLIB#         IUCOMH          .VERSION KERNVAX  1.07  801117
; ORIG.  J. BETTELS     17/11/80
; MOD.   J. VORBRUEGGEN 18/5/83
;
; INTEGER FUNCTION IUCOMH(STRING1, STRING2, NCH)
; Compare the two  strings of NCH bytes length and return a value of
;
;               -1  if  STRING1  < STRING2
;                0               =
;               +1               >
;
; which is determined by the first non-identical character.
; N. B.: The strings are NOT of type CHARACTER !
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  IUCOMH,^M<R2,R3>

        CMPC3   @12(AP),@4(AP),@8(AP)   ;do the compare
        BEQL    10$                     ;equal ?
        BLSS    20$                     ;STRING1<STRING2 ?
        MOVL    #1,R0                   ;STRING1>STRING2
        RET

10$:    CLRL    R0
        RET

20$:    MOVL    #-1,R0
        RET
        .END
