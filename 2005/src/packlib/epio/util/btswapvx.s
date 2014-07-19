 .TITLE BTSWAP
;
;     CALL BTSWAP(IARRAY,NBYTES)
;
;    PURPOSE:      TO SWAP NBYTES OF THE ARRAY IARRAY.
;                  IF NBYTES IS ODD JUST (NBYTES-1) BYTES WILL BE SWAPPED.
;                  CAN BE CALLED FROM FORTRAN, MACRO, PASCAL, PLI, BASIC,
;                  COBOL (MAY BE ALSO BLISS) WITHOUT MODS.
;
;                  IF NBYTES IS ODD THEN ONLY (NBYTES-1) BYTES WILL BE SWAPPED.
;                  IF NBYTES IS LESS THAN OR EQUAL TO ZERO NOTHING IS DONE.
;
;    RESTRICTIONS: NONE
;
;    AUTHOR:       A. PETRILLI/DD  8 JUNE 1984
;
;    REVISIONS:    F. CARMINATI/DD 25 APRIL 1986
;                       .WORD CHANGED TO .ENTRY
;
;
; DECLARATIONS:
;
IARRAY  =     4           ; THESE ARE THE OFFSETS OF ADDRESSES OF
NBYTES  =     8           ; VARIABLES IN THE ARGUMENT LIST WITH
                          ; RESPECT TO THE ARGUMENT POINTER
;
; EXTERNAL ENTRY POINT:
;
     .ENTRY BTSWAP, ^M<R2,R3,R4,R5>   ; REGISTERS TO BE SAVED
                                      ;
     MOVAL @IARRAY(AP), R0            ; GET ADDRESS OF THE ARRAY
     ASHL  #-1, @NBYTES(AP), R1       ; GET NBYTES AND TRANSFORM IT IN A
                                      ; WORD COUNT
                                      ;
     BLEQ  JOB_DONE                   ; PROTECT AGAINST USER MISTAKES
                                      ;
     ASHL  #-1, R1, R2                ; GET THE LONG WORD COUNT IN R2
                                      ;
     BLEQ  SKIP_LOOP                  ; SKIP THE LOOP IF THERE ARE NOT LONG
                                      ; WORDS TO BE SWAPPED
                                      ;
INIT_LOOP:                            ; INITIALIZE LOOP CONTROL VARIABLE
     CLRL  R3                         ; SET R3 TO 0 BEFORE START LOOP
                                      ;
LOOP:                                 ; START OF LOOP
                                      ;
     ROTL  #8, (R0), R4               ; GET NEXT LONG WORD TO BE PROCESSED
                                      ; AND ROTATE IT OF 1 BYTE TO THE LEFT
     ROTL  #16, R4, R5                ; GET IT IN THE RIGHT POSITION FOR
     BICL2 #^X00FF00FF, R4            ; NEXT MASKING THE BYTES OF R4 ARE ZEROED
     BICL2 #^XFF00FF00, R5            ; THE BYTES OF R5 CORRESPONDING TO 'FF'
                                      ;   ARE ZEROED
     BISL3  R4, R5, (R0)+             ; R4 & R5 ARE ORED TOGETHER AND STORED
                                      ; IN ARRAY
     AOBLSS R2, R3, LOOP              ; LOOP ON ALL LONG WORDS OF ARRAY
                                      ;
SKIP_LOOP:                            ; LOOP TERMINATED
     BLBC  R1, JOB_DONE               ; SKIP THIS SECTION IF NBYTES WAS A WHOLE
                                      ; NUMBER OF LONG WORDS
     MOVB  (R0)+, R4                  ; GET FIRST BYTE OF LAST WORD
     MOVB  (R0), B^-1(R0)             ; GET LAST BYTE AND PUT IT INTO THE LAST
                                      ; BUT ONE
     MOVB  R4, (R0)                   ; PUT THE LAST BYTE IN PLACE
                                      ;
JOB_DONE:
     RET                              ; END OF BTSWAP
     .END
