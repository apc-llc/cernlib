;
; $Id: jobnam.s,v 1.1.1.1 1996/02/15 17:50:28 mclareni Exp $
;
; $Log: jobnam.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:28  mclareni
; Kernlib
;
;
 .TITLE JOBNAM
;++
; CERN PROGLIB# Z100    JOBNAM          .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 6/6/83
;
; SUBROUTINE JOBNAM(NAME)
; Return the current process' name in NAME.
;
; If NAME is passed by descriptor and is a string,
; its descriptor is used as an argument to SYS$GETJPI;
; otherwise, it is assumed to be a REAL*8 variable, and
; the process' name is truncated to eight bytes length.
; Normally, NAME should be a CHARACTER*15 variable.
;
; If used as an integer function, function value will
; be the return status of GETJPI.
;--
        .IDENT  /01/

        $JPIDEF         ; get JPI symbol definitions

        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  JOBNAM,^M<>

        MOVAL   @4(AP),R0               ; get address of argument/descriptor
        CMPB    #14,2(R0)               ; is it of string type ?
        BNEQ    10$
        CMPB    #8,3(R0)                ; is it of correct class ?
        BLEQ    10$
        MOVW    (R0),BUFLEN             ; it's a descriptor, set up length
        MOVAL   @4(R0),BUFADR           ; and address
        BRB     20$

10$:    MOVW    #8,BUFLEN               ; force length to be eight bytes
        MOVAL   (R0),BUFADR             ; set up address of argument
20$:    $GETJPI_G -                     ; get the process' name
                ARGLST
        RET

        .PSECT  $PDATA,PIC,CON,REL,SHR,RD,NOWRT,NOEXE,LONG

ARGLST: .LONG   7                       ; argument list for SYS$GETJPI
        .LONG   0[3]
        .ADDRESS -
                ITEMS
        .LONG   0[3]

        .PSECT  $LOCAL,PIC,CON,REL,NOSHR,RD,WRT,NOEXE,LONG
ITEMS:
BUFLEN: .BLKW   1                       ; we put in the length here
        .WORD   JPI$_PRCNAM             ; we want to have the process' name
BUFADR: .BLKL   1                       ; we put in the buffer address here
        .LONG   0[2]

        .END
