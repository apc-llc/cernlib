*
* $Id: rndm2ml.s,v 1.1.1.1 1996/04/01 15:03:05 mclareni Exp $
*
* $Log: rndm2ml.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:05  mclareni
* Mathlib gen
*
*
#if defined(CERNLIB_OLD)
#if (defined(CERNLIB_VAX))&&(!defined(CERNLIB_FORTRAN))
 .TITLE  RNDM2
;
;       UNIFORM RANDOM NUMBER GENERATOR FOR VAX 11-780
;       REWRITTEN FROM CERN IBM 370 VERSION
;       WITH THE IBM-RANDOMNUMBERSEQUENCE
;       TWO PARAMETER VERSION (MCGN,SRGN)
;       ADAPTED AT WUPPERTAL BY H.FORSBACH, MARCH 82
;                       LAST MODIFICATION : JUNE  82
;
MCGN:   .LONG   ^D12345
SRGN:   .LONG   ^D01073
RNDM2:: .WORD   ^M<R2>          ;SAVE REG R2
        MOVL    SRGN,R0         ;MOVE SRGN -> R0
        MOVL    R0,R1           ;MOVE R0 -> R1
        EXTZV   #15,#17,R1,R2   ;\
        MOVL    R2,R1           ;_SHIFT RIGHT R1 -> R1 (15 BITS)
        XORL2   R1,R0           ;EXCLUSIVE OR OF R1,R0 -> R0
        MOVL    R0,R1           ;MOVE R0 -> R1
        EXTZV   #0,#15,R1,R2    ;\
        ROTL    #17,R2,R1       ;_SHIFT LEFT R1 -> R1 (17 BITS)
        XORL2   R1,R0           ;EXCLUSIVE OR OF R1,R0 -> R0
        MOVL    R0,SRGN         ;SAVE THE NEW SRGN
        MOVL    MCGN,R2         ;GET MCGN -> R2
        MULL2   #^D69069,R2     ;69069*R2 -> R2
        MOVL    R2,MCGN         ;SAVE NEW MCGN
        XORL2   R0,R2           ;EXCLUSIVE OR OF R0 [SRGN], R1 [MCGN] -> R2
        EXTZV   #8,#24,R2,R1    ;MANTISSA INTO R1
        CVTLF   R1,R2           ;MANTISSA TO VAX-FLOATING
        EXTZV   #7,#5,R2,R1     ;GET NORMALIZATION SHIFT
        ADDL2   #^X68,R1        ;ADD 128-EXCESS AND AJUST
        INSV    R1,#7,#8,R2     ;PACK EXPONENT INTO R2
        MOVL    R2,R0           ;COPY ONTO R0 (RNDM2)
        RET
IRNDM2::
;
;       UNIFORM DISTRIBUTED POSITIVE INTEGERS
;       WITH THE SAME SEQUENCE AS ON IBM 370 !
;
        .WORD   ^M<R2>          ;\
        MOVL    SRGN,R0         ; \
        MOVL    R0,R1           ;  \
        EXTZV   #15,#17,R1,R2   ;   \
        MOVL    R2,R1           ;    \
        XORL2   R1,R0           ;     \
        MOVL    R0,R1           ;      \
        EXTZV   #0,#15,R1,R2    ;       SEE COMMENTS
        ROTL    #17,R2,R2       ;       OF RNDM2
        MOVL    R2,R1           ;      /
        XORL2   R1,R0           ;     /
        MOVL    R0,SRGN         ;    /
        MOVL    MCGN,R2         ;   /
        MULL2   #^D69069,R2     ;  /
        MOVL    R2,MCGN         ; /
        XORL2   R0,R2           ;/
        EXTZV   #1,#31,R2,R0    ;CLEAR SIGN BIT AND STORE RESULT
        RET
RD2IN::
        .WORD   ^M<>
;
;       SUBROUTINE RD2IN (MCGN,SRGN)
;       MCGN, SRGN ARE THE STARTING INTEGERS OF RNDM2
;
        MOVL    @4(AP),MCGN
        MOVL    @8(AP),SRGN
        RET
RD2OUT::
        .WORD   ^M<>
;
;       SUBROUTINE RD2OUT (MCGN,SRGN)
;       MCGN, SRGN ARE THE LAST USED INTERGES OF RNDM2
;
        MOVL    MCGN,@4(AP)
        MOVL    SRGN,@8(AP)
        RET
#endif
#if defined(CERNLIB_IBM)
*       UNIFORM RANDOM NUMBER GENERATOR FOR IBM 370
*       TWO-PARAMETER VERSION (ORIGINAL)
*       G.MARSAGLIA, K.ANANTHANARAYANAN, N.PAUL. MCGILL UNIV., MONTREAL
*       ADAPTED AT CERN BY T.LINDELOF, SEPT 1977
*       MODIFIED BY T LINDELOF AUG 1978
RNDM2  START 0               R=RNDM2(DUMMY)
#endif
#if defined(CERNLIB_QMIBMXA)
RNDM2  AMODE ANY
RNDM2  RMODE ANY
#endif
#if defined(CERNLIB_IBM)
       ENTRY RD2IN                 .CALL RD2IN(I1,I2)
       ENTRY RD2OUT                .CALL RD2OUT(I1,I2)
       ENTRY IRNDM2                .K=IRNDM2(DUMMY)
REGB   EQU    1
REGC   EQU    2
REGD   EQU    3
*
*     R=RNDM2(DUMMY)          RESULT IS NORMALIZED FLOATING POINT VALUE
*                             UNIFORMLY DISTRIBUTED ON (0.0,1.0).
       USING *,15
       STM   REGB,REGD,24(13) SAVE REGISTERS 1,2,3
RDIGT1 L     REGB,SRGN        LOAD SRGN INTO REGB
       LR    REGC,REGB        AND INTO REGC
       SRL   REGC,15          SHIFT REGC RIGHT 15 BITS
       XR    REGB,REGC        AND XOR INTO REGB
       LR    REGC,REGB        COPY REGB INTO REGC
       SLL   REGC,17          SHIFT IT LEFT 17 BITS,
       XR    REGB,REGC        AND XOR INTO REGB
       ST    REGB,SRGN        SAVE THE NEW 'SRGN'
       L     REGD,MCGN        LOAD MCGN INTO REGD
       M     REGC,MULT        AND MULTIPLY BY 69069
       ST    REGD,MCGN        STORE RESULT,MODULO 2**32, AS NEW 'MCGN'
       XR    REGD,REGB        XOR NEW 'MCGN' AND 'SRGN' IN REGD
       SRL   REGD,8           SHIFT REGD RIGHT 8 BITS FOR F.P. FRACTION
       AL    REGD,CHAR        ADD CHARACTERISTIC X'40' INTO FIRST BYTE
       ST    REGD,FWD         STORE AT FWD, LOAD INTO FPR 0,
       LE    0,FWD            AND ADD NORMALIZED TO ZERO
       AE    0,Z              LEAVING RESULT IN FPR 0
RETRN1 LM    REGB,REGD,24(13)
       BCR   15,14            RETURN
*
*     K=IRNDM2(DUMMY)         UNIFORMLY DISTRIBUTED POSITIVE INTEGER.
*
       USING IRNDM2,15
IRNDM2 STM   REGB,REGD,24(13) SAVE REGISTERS 1,2,3
RDIGT5 L     REGB,SRGN        LOAD SRGN INTO REGB
       LR    REGC,REGB        AND INTO REGC
       SRL   REGC,15          SHIFT REGC RIGHT 15 BITS
       XR    REGB,REGC        AND XOR INTO REGB
       LR    REGC,REGB        COPY REGB INTO REGC
       SLL   REGC,17          SHIFT IT LEFT 17 BITS,
       XR    REGB,REGC        AND XOR INTO REGB
       ST    REGB,SRGN        SAVE THE NEW 'SRGN'
       L     REGD,MCGN        LOAD MCGN INTO REGD
       M     REGC,MULT        AND MULTIPLY BY 69069
       ST    REGD,MCGN        STORE RESULT,MODULO 2**32, AS NEW 'MCGN'
       XR    REGD,REGB        XOR NEW 'MCGN' AND 'SRGN' IN REGD
       SRL   REGD,1           SHIFT LEFT 1 BIT,LEAVING SIGN BIT ZERO
       LR    0,REGD           AND MOVE RESULT TO GPR 0
RETRN5 LM    REGB,REGD,24(13)
       BCR   15,14            RETURN
*
*   CALL RD2IN(I1,I2)         I1,I2 ARE USED FOR STARTING THE TWO
*                             SEQUENCES 'MCGN' AND 'SRGN'.
       USING RD2IN,15
RD2IN  STM   REGB,REGD,24(13) SAVE REGISTERS 1,2,3
       LM    REGC,REGD,0(1)   LOAD ADDRESSES OF I1,I2 INTO REGC,REGD
       L     REGC,0(REGC)     LOAD VALUE OF I1 INTO REGC
       LTR   REGC,REGC
ST1    ST    REGC,MCGN        STORE AT 'MCGN'
       L     REGD,0(REGD)     LOAD I2 INTO REGD
       LTR   REGD,REGD
ST2    ST    REGD,SRGN        AND STORE AT 'SRGN'.
RETRN0 LM    REGB,REGD,24(13) RESTORE REGISTERS 1,2,3
       BCR   15,14            AND RETURN
*
*         CALL RD2OUT(I1,I2)
*
          USING RD2OUT,15
RD2OUT    STM   REGB,REGD,24(13)   SAVE REGISTERS 1,2,3
          LM     REGC,REGD,0(1)    LOAD ADDR. OF I1,I2 INTO REGC,REGD
          MVC    0(4,REGC),MCGN    MOVE MCGN TO I1
          MVC    0(4,REGD),SRGN    MOVE SRGN TO I2
          LM     REGB,REGD,24(13)  RESTORE REGISTERS 1,2,3
          BCR    15,14             AND RETURN
*
*      CONSTANTS AND STORAGE RESERVATION
*
MCGN   DC     F'12345'
X7FF   DC     X'000007FF'
SRGN   DC     F'01073'
MULT   DC     F'69069'
CHAR   DC     X'40000000'
FWD    DC     F'0'
Z      DC     E'0.0'
       END
#endif
#endif
