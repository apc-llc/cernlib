;
; $Id: ie3tos.s,v 1.1.1.1 1996/02/15 17:50:27 mclareni Exp $
;
; $Log: ie3tos.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:27  mclareni
; Kernlib
;
;
 .TITLE IE3TOS
;++
; CERN PROGLIB# M220    IE3TOS          .VERSION KERNVAX  2.34  900301
; ORIG.   February 89, M. Jonker/CERN
; CORR.   D. Coffman 12/02/1990
;
; SUBROUTINE IE3TOS (MS,MT,NWDO,JBAD)
; Convert single precision for input with copy
; from source in IEEE to target in native data format
;--
        .IDENT  /900212MJJ/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG

        .ENTRY  IE3TOS,^M<R2,R3,R4>
        MOVQ    4(AP),R0                ;R0/R1 = source/destination
        MOVL    @12(AP),R2              ;AP = count
        MOVL    #0,@16(AP)              ;JBAD=0 initially

        BRB     29$

        .ALIGN  LONG

20$:    ROTL    #16,(R0)+,R3
;       extract and test the characteristics
        BICW3   #^X807F,R3,R4
        BEQL    31$
        CMPW    R4,#^X7E80
        BGEQ    41$
        ADDL3   #^X00000100,R3,(R1)+
29$:    SOBGEQ  R2,20$                  ;loop
        RET

        ; Handle exceptions

;     - Zero characteristics, test for floating zero
31$:    BITL    #^XFFFF7FFF,R3
        BNEQ    51$             ;De-normalized number
        CLRL    (R1)+           ;Floating zero
39$:    SOBGEQ  R2,20$          ;Loop
        RET

;     - Characteristics .gt. 253, not a number or overflow
41$:    CMPW    #^X7F80,R4
        BNEQ    42$
        BITL    #^XFFFF007F,R3
        BEQL    42$
;       Not a number
        MOVL    #^X00008001,(R1)+
        SUBL3   R2,@12(AP),@16(AP)
        BRB     39$

;       Signed overflow
42$:    BICL2   #^XFFFF7FFF,R3
        BISL3   #^X00007F81,R3,(R1)+
        SUBL3   R2,@12(AP),@16(AP)
        BRB     39$

;     - Denormalized number
51$:    BBCC    #6,R3,53$
;       Exponent = 2
        ROTL    #1,R3,R3
        BBCC    #16,R3,52$      ;Test sign bit
        BBSS    #15,R3,52$      ;Restore sign bit
52$:    BISL3   #^X00000100,R3,(R1)+
        BRB     39$
53$:    BBC     #5,R3,55$
;       Exponent = 1
        BBCC    #15,R3,54$      ;Test sign bit
        BBSS    #13,R3,54$      ;Restore sign bit
54$:    ROTL    #2,R3,(R1)+
        BRB     39$
;       Underflow
55$:    CLRL    (R1)+
        BRB     39$

;!      exceptions:
;!       c=0,     m=0  floating zero
;!       c=0,     m#0  de-normalized number +/- 0.m 2**(-126 or -1022)
;!       c=1...1  m=0  signed infinity
;!       c=1...1  m#0  not a number
        .END
