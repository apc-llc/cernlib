*
* $Id: rndm.s,v 1.1.1.1 1996/02/15 17:54:41 mclareni Exp $
*
* $Log: rndm.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:41  mclareni
* Kernlib
*
*
RNDM     CSECT
*
* CERN PROGLIB# V104    RNDM            .VERSION KERNIBX  1.03  910315
*
*       UNIFORM RANDOM NUMBER GENERATOR FOR IBM 370
*       G.MARSAGLIA, K.ANANTHANARAYANAN, N.PAUL. MCGILL UNIV., MONTREAL
*       ONE-PARAMETER VERSION
*       ADAPTED AT CERN BY T.LINDELOF, SEPT 1977
*       RDMIN MODIFIED BY T LINDELOF AUG 1978
         ENTRY _rndm_          R=RNDM(DUMMY)
         ENTRY _rdmin_         CALL RDMIN(I1)
         ENTRY _rdmout_        CALL RDMOUT(I1)
         ENTRY _irndm_         K=IRNDM(DUMMY)
*
*     R=RNDM(DUMMY)          RESULT IS NORMALIZED FLOATING POINT VALUE
*                            UNIFORMLY DISTRIBUTED ON (0.0,1.0).
         USING *,15
_rndm_   STM   2,1,16(13)    SAVE REGISTERS
         L     12,=A(MCGN)
         USING MCGN,12
         L     3,MCGN        LOAD MCGN INTO REGD
         M     2,MULT        AND MULTIPLY BY 69069
         ST    3,MCGN        STORE RESULT,MODULO 2**32, AS NEW 'MCGN'
         SRL   3,8           SHIFT 3 RIGHT 8 BITS FOR F.P. FRACTION
         AL    3,CHAR        ADD CHARACTERISTIC X'40' INTO FIRST BYTE
         ST    3,FWD         STORE AT FWD, LOAD INTO FPR 0,
         LE    0,FWD         AND ADD NORMALIZED TO ZERO
         AE    0,Z           LEAVING RESULT 'RNDM' IN FPR 0.
RETRN1   LM    2,1,16(13)
         BCR   15,14         RETURN
*
*     K=IRNDM(DUMMY)         UNIFORMLY DISTRIBUTED POSITIVE INTEGER.
*
         USING *,15
_irndm_  STM   2,1,16(13)    SAVE REGISTERS
         L     12,=A(MCGN)
         L     3,MCGN        LOAD MCGN INTO 3
         M     2,MULT        AND MULTIPLY BY 69069
         ST    3,MCGN        STORE RESULT,MODULO 2**32, AS NEW 'MCGN'
         SRL   3,1           SHIFT LEFT 1 BIT,LEAVING SIGN BIT ZERO
         LR    0,3           AND MOVE RESULT 'IRNDM' TO GPR 0.
RETRN5   LM    2,1,16(13)
         BCR   15,14         RETURN
*
*   CALL RDMIN(I1)           I1 IS USED FOR STARTING THE
*                            SEQUENCE 'MCGN'
         USING *,15
_rdmin_  STM   2,1,16(13)    SAVE REGISTERS
         L     12,=A(MCGN)
         LR    2,0           address of argument
         L     2,0(0,2)      LOAD VALUE OF I1 INTO REGC
         LTR   2,2
ST1      ST    2,MCGN        STORE AT 'MCGN'
RETRN0   LM    2,1,16(13)    RESTORE REGISTERS
         BCR   15,14         AND RETURN
*
*         CALL RDMOUT(I1)
*
         USING *,15
_rdmout_ STM   2,1,16(13)    SAVE REGISTERS
         L     12,=A(MCGN)
         LR    2,0           address of argument
         MVC   0(4,2),MCGN   MOVE MCGN TO I1
         LM    2,1,16(13)    RESTORE REGISTERS
         BCR   15,14             AND RETURN
*
*      CONSTANTS AND STORAGE RESERVATION
*
         LTORG               ... adcon must be in CSECT
DATA     PSECT
MCGN     DC     F'12345'
X7FF     DC     X'000007FF'
MULT     DC     F'69069'
CHAR     DC     X'40000000'
FWD      DC     F'0'
Z        DC     E'0.0'
         END
