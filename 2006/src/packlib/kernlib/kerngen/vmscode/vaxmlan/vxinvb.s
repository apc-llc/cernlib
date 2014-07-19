;
; $Id: vxinvb.s,v 1.1.1.1 1996/02/15 17:50:31 mclareni Exp $
;
; $Log: vxinvb.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:31  mclareni
; Kernlib
;
;
 .TITLE VXINVB
;++
; CERN PROGLIB# M434    VXINVB          .VERSION KERNVAX  2.31  890323
; ORIG.  12-June-87, J. Zoll + F. Carminati
; CORR.   August 88, M. Jonker/CERN, speed-up;  Feb.89 test on zero
;
; SUBROUTINE VXINVB (MV,N)
; VX invert bytes, in-place
; Invert in-situ the 4 bytes in each long-word of MV, N words
;--
        .IDENT  /03MJJ/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VXINVB,^M<R2,R3,R4,R5,R6>
        MOVQ    4(AP),R0                ;R0 = source pointer
                                        ;R1 = adr(count)
        MOVL    (R1),R1                 ;R1 = count
        BLEQ    99$                     ; exit if zero / -ve
        ASHL    #-1,R1,AP               ;AP = count/2
        MOVL    #^X00FF00FF,R6          ;R6 = 0 | F | 0 | F  mask
        BLBC    R1,20$            ;Test for odd number of swaps

;    - Odd number of swaps
        ADDL    #1,AP
        MOVL    (R0),R3
        BRB     21$

;    Loop optimization:
;    The loop is longword aligned and two items are swapped per cycle
    .ALIGN    LONG
20$:    MOVQ    (R0),R2                 ;R2  = 1 | 2 | 3 | 4

        ROTL    #24,R2,R4               ;R4  = 4 | 1 | 2 | 3
        BICL2   R6,R4                   ;R4  = 4 | 0 | 2 | 0
        BICL2   R6,R2                   ;R2  = 1 | 0 | 3 | 0
        ROTL    #8,R2,R2                ;R2  = 0 | 3 | 0 | 1
        BISL3   R4,R2,(R0)+             ;(R0)= 4 | 3 | 2 | 1

21$:    ROTL    #24,R3,R4               ;R4  = 4 | 1 | 2 | 3
        BICL2   R6,R4                   ;R4  = 4 | 0 | 2 | 0
        BICL2   R6,R3                   ;R3  = 1 | 0 | 3 | 0
        ROTL    #8,R3,R3                ;R3  = 0 | 3 | 0 | 1
        BISL3   R4,R3,(R0)+             ;(R0)= 4 | 3 | 2 | 1

        SOBGTR  AP,20$                  ;loop
99$:    RET
        .END
