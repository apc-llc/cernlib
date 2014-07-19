;
; $Id: locb.s,v 1.1.1.1 1996/02/15 17:50:28 mclareni Exp $
;
; $Log: locb.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:28  mclareni
; Kernlib
;
;
 .TITLE LOCB
;++
; CERN PROGLIB# N100    LOCB            .VERSION KERNVAX  2.33  891120
; ORIG.  17/11/89 JZ
;
; INTEGER FUNCTION LOCB(AM)
; Return the virtual address of AM in longwords
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  LOCB,^M<>

        MOVL    4(AP),R0
        RET
        .END
