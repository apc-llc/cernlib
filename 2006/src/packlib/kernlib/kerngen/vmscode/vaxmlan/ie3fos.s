;
; $Id: ie3fos.s,v 1.1.1.1 1996/02/15 17:50:27 mclareni Exp $
;
; $Log: ie3fos.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:27  mclareni
; Kernlib
;
;
 .TITLE IE3FOS
;++
; CERN PROGLIB# M220    IE3FOS          .VERSION KERNVAX  2.34  900301
; ORIG.   February 89, M. Jonker/CERN
; CORR.   D. Coffman 12/02/1990
;
; SUBROUTINE IE3FOS (MS,MT,NWDO,JBAD)
; Convert single precision for output with copy
; from source in native to target in IEEE data format
;--
        .IDENT  /900212MJJ/     ;M.Jonker CERN/EP
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG

        .ENTRY  IE3FOS,^M<R2,R3,R4>
        MOVQ    4(AP),R0                ;R0/R1 = source/destination
        MOVL    @12(AP),R2              ;AP = count
        MOVL    #0,@16(AP)              ;JBAD=0 initially

        BRB     29$

        .ALIGN  LONG

20$:    ROTL    #16,(R0)+,R3
;       extract and test the characteristics
        BICL3   #^X807FFFFF,R3,R4
        CMPL    R4,#^X01000000
        BLEQ    21$
        CMPL    R4,#^X7F800000
        BEQL    31$
        SUBL3   #^X01000000,R3,(R1)+
        SOBGEQ  R2,20$                  ;loop
        RET

21$:    ; Handle exceptions
        BICL3   #^X007FFFFF,R3,R4
        BNEQ    22$
;     - Floating zero:
        CLRL    (R1)+
29$:    SOBGEQ  R2,20$                  ;loop
        RET

22$:    CMPL    #^X80000000,R4
        BNEQ    23$
;       Reserved: translate into not-a-number
        MOVL    #^X7F80FE00,(R1)+
        SUBL3   R2,@12(AP),@16(AP)
        BRB     29$

;       The characteristics is 1 or 2, we can translate this into a
;       de-normalized number only.
;       - The word is first shift to the left to make room to store
;         the bit that cannot be implied anymore.
23$:    ASHL    #-1,R3,R4       ;First shift to the left
;       - Set this bit on and test if the charteristics is 2
        BBCS    #22,R4,24$
;       - The characteristics is 1, we need an extra shift
        ASHL    #-1,R4,R4
24$:    BICL3   #^X7F800000,R4,(R1)+
        BRB     29$

;       exponent 7F8 : signed overflow
31$:    BICL3   #^X007FFFFF,R3,(R1)+
        SUBL3   R2,@12(AP),@16(AP)
        BRB     29$
        .END
