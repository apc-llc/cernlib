 .TITLE W16MOV
;
;     CALL W16MOV(SOURCE,N1,TARGET,N2,N3)
;
;     PURPOSE: TO MOVE N3 16 BITS WORDS FROM SOURCE(N1) TO TARGET(N2)
;              CAN BE CALLED FROM FORTRAN, MACRO, PASCAL, PLI, BASIC,
;              COBOL (MAY BE ALSO BLISS) WITHOUT MODS
;
;     RESTRICTIONS: N3 MUST BE .LE. 2**31-1 (2*10**9)
;
;     AUTHOR: A. PETRILLI/DD  29 MARCH 1984
;
;
;
; DECLARATIONS:
SOURCE  =     4  ; THESE ARE THE OFFSETS OF
N1      =     8  ; ADDRESSES OF VARIABLES IN
TARGET  =    12  ; THE ARGUMENT LIST WITH
N2      =    16  ; RESPECT TO THE
N3      =    20  ; ARGUMENT POINTER
;
;
; ENTRY POINT:
        .ENTRY W16MOV,^M<R2,R3,R4,R5>  ; R3,R4,R5 NEED TO BE SAVED BECAUSE
                                       ;   THEY ARE USED BY MOVC3
                               ;
        SUBL3 #1, @N1(AP), R0  ; PUT N1-1 INTO R0 AND
        ASHL  #1, R0, R0       ;   CONVERT THE WORD OFFSET OF SOURCE
                               ;   TO A BYTE OFFSET
                               ;
        SUBL3 #1, @N2(AP), R1  ; PUT N2-1 INTO R1 AND
        ASHL  #1, R1, R1       ;   CONVERT THE WORD OFFSET OF TARGET
                               ;   TO A BYTE OFFSET
                               ;
        ASHL  #1, @N3(AP), R2  ; PUT N3*2 INTO R2 SO THE WORD COUNT
                               ;   IS NOW A BYTE COUNT
                               ;
        BLEQ  JOB_DONE         ; PROTECT AGAINST USER MISTAKES
                               ;
        MOVC3 R2, @SOURCE(AP)[R0], @TARGET(AP)[R1]
                               ; HERE WE REALLY DO THE JOB:
                               ;   R2 BYTES ARE COPIED FROM
                               ;   SOURCE(R0) TO TARGET(R1)
                               ;
JOB_DONE:
        RET                    ; END OF W16MOV
	.END
