 .TITLE VAX_IBMCONV
;
;       IBM TO VAX IN PLACE CONVERSION FOR FLOATING POINT R*4
;
;       CALL CVT360(A(I),A(J),MODE) CONVERTS R*4 WORDS A(I) THROUGH A(J)
;                                       FROM IBM TO VAX FORMAT
;
;       CALL FTO360(A(I),A(J),MODE) CONVERTS FROM VAX TO IBM FORMAT
;
;       MODE SPECIFIES TYPE OF CONVERSION
;       = 0 OR MISSING FLOATING POINT
;       = 1            I*4 (16-16 BIT SWAP IS PERFORMED)
;       = 2            I*2 (WITH SIGN EXTENSION)
;
;       J >= I IS A REQUIREMENT.
;
;       THE ARRAY A MUST BE CORRECTLY ALIGNED ON A R*4 BOUNDARY
;
;       BYTE SWAPPING IS PERFORMED BY THIS ROUTINE
;
;       A.F.R. VERSION 2 9-9-81 SWAP DONE HERE, THREE MODES INTRODUCED
;       A.F.R. VERSION 1  20-3-81 FLOATING WITH NO BYTE SWAP
;
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  FTO360,^M<R2,R3,R4,R5,R6>
        MOVL    4(AP),R2       ;R2 = FIRST BYTE OF CURRENT R*4 WORD
        MOVL    (AP),R1        ;THIS IS THE NUMBER OF ARGUMENTS
        CMPL    R1,#3          ;IF 3 THEN MODE IS SENT
        BNEQ    100$           ;DEFAULT IS FLOATING
        CASEL   @12(AP),#0,#2   ;HERE ARE THE THREE CASES
10$:    .WORD   100$-10$       ;FLOATING
        .WORD   200$-10$       ;I*4 INTEGERS
        .WORD   300$-10$       ;I*2 INTEGERS
100$:
        CMPL   R2,8(AP)      ;CHECK FOR END OF ARRAY
        BLEQU   105$
        RET
105$:
        ROTL    #16,(R2),R3     ;GET NEXT WORD ALIGNED NICELY
        BEQL    115$           ;SKIP IF IT IS ZERO
        EXTZV   #0,#23,R3,R4    ;EXTRACT MANTISSA
        BISL2   #^X00800000,R4  ;SET THE HIDDEN LEADING BIT
        EXTZV   #23,#8,R3,R5    ;EXTRACT EXPONENT
        SUBL2   #129,R5         ;TAKE AWAY EXCESS
        EXTZV   #0,#2,R5,R6     ;GET REMAINDER
        ASHL    #-2,R5,R5       ;POWERS OF 64
        ADDL2   #^X41,R5        ;ADD EXCESS 64 FOR EXPONENT
        BICB2   #^X80,R5
        BBC     #31,R3,110$     ;CHECK FOR NEGATIVE DATA
        BISB2   #^X80,R5
110$:
        SUBL2   #3,R6           ;SET MANTISSA SHIFT
        ASHL    R6,R4,R3        ;MANTISSA TO IBM STANDARD
        INSV    R5,#24,#8,R3    ;NOW HAVE IT ALL IN R3
115$:
        ROTL    #16,R3,(R2)+   ;HERE IS THE SWAP
        BRW     100$
200$:
        CMPL    R2,8(AP)        ;CHECK FOR END OF ARRAY
        BLEQU   205$
        RET
205$:
        ROTL    #16,(R2),(R2)+  ;DO 16-16 SWAP
        BRW     200$
300$:
        CMPL    R2,8(AP)        ;CHECK FOR END OF ARRAY
        BLEQU   305$
        RET
305$:
        CVTWL  (R2),(R2)+              ;SIGN EXTEND THE HIGH 16 BITS
        BRW     300$
;
;
        .ENTRY  CVT360,^M<R2,R3,R4,R5>
        MOVL    4(AP),R2       ;MOVE WORD POINTER TO R2
        MOVL    (AP),R1        ;THIS IS THE NUMBER OF ARGUMENTS
        CMPL    R1,#3          ;IF 3 THEN MODE IS SENT
        BNEQ    1100$           ;DEFAULT IS FLOATING
        CASEL   @12(AP),#0,#2   ;HERE ARE THE THREE CASES
1010$:    .WORD   1100$-1010$       ;FLOATING
        .WORD   1200$-1010$       ;I*4 INTEGERS
        .WORD   1300$-1010$       ;I*2 INTEGERS
1100$:
        CMPL    R2,8(AP)       ;ARE WE DONE?
        BLEQU   1105$
        RET
1105$:
        ROTL    #16,(R2),R3         ;MOVE NEXT WORD TO R3
        BEQL    1110$           ;BRANCH IF ZERO, NO CONVERSION
        BICL3   #^XFF000000,R3,R5       ;EXTRACT MANTISSA
        EXTZV   #24,#7,R3,R4    ;EXTRACT EXPONENT
        SUBL2   #^X41,R4        ;TAKE AWAY EXCESS
        ASHL    #2,R4,R4        ;POWERS OF TWO
        CVTLF   R5,R3           ;MANTISSA TO VAX FLOATING
        EXTZV   #7,#5,R3,R5     ;GET NORMALIZATION SHIFT
        ADDL2   R5,R4           ;NEW EXPONENT
        ADDL2   #^X6C,R4        ;ADD 128 EXCESS AND ADJUST
        INSV    R4,#7,#8,R3     ;PACK EXPONENT INTO R3
        BBC     #15,(R2),1110$  ;WAS INPUT NEGATIVE
        BISL2   #^X00008000,R3  ;YES, SET SIGN
1110$:
        MOVL    R3,(R2)+        ;STORE IT
        BRW     1100$
1200$:
        CMPL    R2,8(AP)        ;CHECK FOR END OF ARRAY
        BLEQU   1205$
        RET
1205$:
        ROTL    #16,(R2),(R2)+  ;DO 16-16 SWAP
        BRW     1200$
1300$:
        CMPL    R2,8(AP)        ;CHECK FOR END OF ARRAY
        BLEQU   1305$
        RET
1305$:
        CVTWL  (R2),(R2)+              ;SIGN EXTEND THE HIGH 16 BITS
        BRW     1300$
        .END
