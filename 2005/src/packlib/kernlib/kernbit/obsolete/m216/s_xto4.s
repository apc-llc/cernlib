*
* $Id: s_xto4.s,v 1.1.1.1 1996/02/15 17:47:37 mclareni Exp $
*
* $Log: s_xto4.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:37  mclareni
* Kernlib
*
*
S$XTO4   CSECT
#if defined(CERNLIB_QMIBMXA)
S$XTO4   AMODE ANY
S$XTO4   RMODE ANY
#endif
         PRINT NOGEN
         USING *,15
         B     ROUNDSA
         DC    X'07'
         DC    CL7'S$XTO4'
SAVEAREA DS    18F
ROUNDSA  STM   14,12,12(13)
         LR    12,13
         LA    13,SAVEAREA
         ST    13,8(12)
         ST    12,4(,13)
         USING SAVEAREA,13
         DROP  15
         LM    2,3,0(1)   GET ADDRESS OF INPUT & OUTPUT ARRAYS
         LA    5,256(0)     GET LENGTH OF ARRAY/2
         LA    4,4(0)       GET NR TO SHIFT
LOOP     EQU   *
         LM    10,12,0(2)      GET FIRST WORD
         SRDL  10,0(4)      SHIFT
         STM   10,11,0(3)   STORE RESULT
         NI    0(3),X'0F'   AND OUT TOP 4 BITS
         LA    3,8(3)       UPDATE ADDRESS OF OUTPUT
         MVC   0(8,3),7(2)  MOVE SECOND WORD
         NI    0(3),X'0F'   AND OUT TOP FUR BITS
         LA    2,15(2)      UPDATE POINTER
         LA    3,8(3)       UPDATE ADDR OF OUTPUT
         BCT   5,LOOP       LOOP OVER?
END      L     13,4(,13)
         LM    2,12,28(13)
         L     14,12(,13)
**       MVI   12(13),X'FF'
         BR    14
         END
