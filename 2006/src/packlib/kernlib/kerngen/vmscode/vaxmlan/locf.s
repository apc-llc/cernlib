;
; $Id: locf.s,v 1.1.1.1 1996/02/15 17:50:28 mclareni Exp $
;
; $Log: locf.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:28  mclareni
; Kernlib
;
;
 .TITLE LOCF
;++
; CERN PROGLIB# N100    LOCF            .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 19/5/83
;
; INTEGER FUNCTION LOCF(AM)
; Return the virtual address of AM in longwords
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  LOCF,^M<>

        ASHL    #-2,4(AP),R0
        RET
        .END
