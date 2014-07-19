*
* $Id: cvt76a.s,v 1.1.1.1 1996/02/15 17:47:37 mclareni Exp $
*
* $Log: cvt76a.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:37  mclareni
* Kernlib
*
*
NAME     TITLE 'TITLE'
CVT76A   CSECT               CONVERTS FROM CDC REAL FORMAT TO
#if defined(CERNLIB_QMIBMXA)
CVT76A   AMODE ANY
CVT76A   RMODE ANY
#endif
         PRINT  DATA         IBM REAL FORMAT
*
*  CALLED FROM FORTRAN AS
*    CALL CVT76A(DATA,N) WHERE DATA IS STRING OF N REAL*8
*                        CDC REPRESENTATION FLOATING POINT
*                        OR INTEGER WORDS. RESULT IS PUT BACK
*                        IN ARRAY DATA. FLOATING POINT NUMBERS
*                        ARE PUT BACK AS REAL*8 WHEREAS INTEGERS
*                        ARE PUT AS INTEGER*4 IN LOWER HALF OF
*                        THE 'REAL*8' WORD. INTEGER OVER/UNDERFLOW
*                        ARE SET TO MAX INTEGER WITH CORRECT SIGN.
*
         USING  CVT76A,15
         B      START             NUMBERS OUT OF RANGE OF IBM
         DC     X'07',CL7'CVT76A' SET TO ZERO OR OVERFLOW
SAVE     DS     18F                AS APPROPRIATE
         SPACE
START    STM    14,12,12(13) SAVE REGISTERS
         LR     12,13
         LA     13,SAVE
         ST     12,4(13)
         ST     13,8(12)
         DROP   15
         USING  SAVE,13
         L      11,4(1)      ADRESS OF N
         L      11,0(11)     VALUE OF N-- ARRAY SIZE
         LTR    11,11        CHECK FOR N GT 0
         BC     12,EPILOG
         SPACE 2
         L      1,0(1)       ADDRESS OF ARRAY
SIGN     EQU    3            SET UP REGISTERS WITH CONSTANTS
R2       EQU    4
R3       EQU    5
X2CD     EQU    6
ZERO     EQU    7
F        EQU    8
E        EQU   10
         SR     7,7
         LM     4,6,CONSTS
LOOP     LH     E,0(1)       LOAD EXPONENT INTO E
         N      E,=X'00000FFF'
         L      F,0(1)       LOAD MANTISSA INTO F AND F+1
         N      F,=X'0000FFFF'
         L      F+1,4(1)
         LR     SIGN,ZERO
         LR     2,E          CHECK FOR NEGATIVE ARG.
         S      2,=XL4'800'
         BM     POSNUM       IF NEG COMPLEMENT WORD
         X      E,=X'00000FFF' AND SET SIGN FLAG
         X      F,=X'0000FFFF'
         X      F+1,=X'FFFFFFFF'
         LA     SIGN,128(SIGN)
POSNUM   LTR    2,E
         BZ     INTEGR       CHECK IF INTEGER
         SR     2,R2         IF EXPONENT NEG. ADD 1
         BNM    POSEXP
         LA     E,1(E)
POSEXP   SR     E,X2CD        CORRECT BIAS OF EXPONENT
         LR     2,E          TEST FOR EXPONENT IN RANGE
         SRA    2,9
         CR     2,ZERO
         BZ    INRNGE
         BH     OVFLOW
UNFLOW   ST     ZERO,0(1)     SET RETURN ARG. TO ZERO
         ST     ZERO,4(1)
         B      NEXT
OVFLOW   L      2,=X'FFFFFFFF' SET RETURN ARG. TO OVERFLOW
         ST     2,4(1)
         CR     SIGN,ZERO
         BNE    NEGOV
         SRL    2,1
NEGOV    ST     2,0(1)
         B      NEXT
INRNGE   LR     2,E          CHANGE EXPONENT TO BASE 16
         NR     2,R3
         N      E,=XL4'FFFC'
         SLDA   F,5(2)         SHIFT MANTISSA
CONT     SRA    E,2
         AR     E,SIGN         PACK SIGN BIT INTO EXP.
GONOW    ST     F,0(1)         PACK EXPONENT AND MANTISSA
         ST     F+1,4(1)       INTO R*8 REPLY
         STC    E,0(1)
         B      NEXT
INTEGR   ST     ZERO,0(1)
         LTR    F,F
         BNZ    TOOLRG
         LTR    F+1,F+1
         BM     TOOLRG
         LTR    SIGN,SIGN
         BZ     GOINT
         LCR    F+1,F+1
         B      GOINT
TOOLRG   L      F+1,=X'7FFFFFFF'
         LTR    SIGN,SIGN
         BZ     GOINT
         L      F+1,=X'80000000'
GOINT    ST     F+1,4(1)
NEXT     LA     1,8(1)         INCREMENT ADRESS POINTER
         BCT    11,LOOP        GO BACK IF ANY MORE
         B      EPILOG
         SPACE  2
         DS     0F
CONSTS   DC     XL4'400',F'3',XL4'2CD'
EPILOG   L      13,SAVE+4      PICK UP OLD SAVE AREA
         L      14,12(13)      RETURN ADDRESS
         LM     2,12,28(13)    RESTORE REG 1 AND 2
**       MVI    12(13),X'FF'   SIGNAL RETURN
         BR     14
         END
