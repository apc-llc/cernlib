 .TITLE EPIO2_MACRO
;
; DISK ROUTINES FOR EPIO I/O PACKAGE
;
                   $RABDEF
;
DKRECL:            .BLKL      100
ADKREC:            .ADDRESS    DKRECL
;
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
                   .ENTRY      EPOPENV_VAX,^M<>
                   $OPEN       FAB=@4(AP)
                   BRB         CONNECT
                   .ENTRY      EPOPENO_VAX,^M<>
                   $CREATE     FAB=@4(AP)
CONNECT:           BLBC        R0,300$
                   $CONNECT    RAB=@8(AP)
                   SUBL3       #1,@12(AP),R1
                   ADDL3       8(AP),#RAB$W_RSZ,@ADKREC[R1]
300$:              RET
;
                   .ENTRY      EPDKRECL,^M<>
                   SUBL3       #1,@4(AP),R1
                   ASHL        #2,R1,R1
                   ADDL2       ADKREC,R1
                   MOVZWL      @(R1),R0
                   RET
;
                   .END
 
