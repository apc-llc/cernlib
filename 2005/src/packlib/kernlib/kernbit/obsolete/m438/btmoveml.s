*
* $Id: btmoveml.s,v 1.1.1.1 1996/02/15 17:47:43 mclareni Exp $
*
* $Log: btmoveml.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:43  mclareni
* Kernlib
*
*
#if defined(CERNLIB_IBM)
BTMOVE   CSECT
#if defined(CERNLIB_QMIBMXA)
BTMOVE   AMODE ANY
BTMOVE   RMODE ANY
#endif
*      CALL    BTMOVE(A,NA,X,NX,NBITS)
*     THIS ROUTINE MOVES A BIT-STRING OF LENGTH NBITS STARTING AT
*     BIT NA OF ARRAY A TO POSITION STARTING AT BIT NX OF ARRAY X.
*     BITS ARE COUNTED LEFT TO RIGHT ACROSS WORD BOUNDARY,START. WITH 1
         USING *,15
         B     BEGIN
         DC    X'6'
         DC    CL6'BTMOVE'
BEGIN    STM  0,10,24(13)
         LM    1,5,0(1)      R1-R5=ADDRESSES OF THE ARG. OF ROUTINE
*      FIND THE ADDRESS OF THE FIRST BYTE OF ARRAY A TO BE MOVED.
         SR     0,0
         SR    7,7
         L     6,0(2)        R6=LA
         SRDL  6,3(0)        R6/8
         SRL   7,29(0)       REST OF DIV.
         CR    7,0
         BH    LANOT0
         L     7,NBYTE
         S     1,ONE
LANOT0   AR    1,6           R1=ADDRESS OF FIRST BYTE OF A TO BE MOVED.
         LR     2,7          R2= 1ST BIT OF BYTE TO BE MOVED.
*      FIND THE ADDRESS OF THE FIRST BYTE OF ARRAY X TO BE CHANGED
         SR    7,7
         L     6,0(4)
         SRDL  6,3(0)
         SRL   7,29(0)
         CR    7,0
         BH    LXNOT0
         L     7,NBYTE
         S     3,ONE
LXNOT0   AR    3,6
         LR    4,7
*
         L     8,WORD
         SR    8,2
         L     9,WORD
         SR    9,4
         L     6,0(5)        R6= NBER OF BITS TO TRANSFER FROM A TO X
         CR    6,0
         BNH   FIN
*   BRANCHING IF THE NBITS TO TRANSFER IS BIGGER THAN 32
         L     10,WORD
         SR    10,6
         BM    NBERW
*      THE NBITS TO BE MOVED ARE LESS THAN 32.
         LR    0,6
         L     5,0(3)
         B     LASTB
*
*      FIND THE NBER OF WORDS TO BE MOVED FROM A TO X.
NBERW    SR    7,7
         SRDL  6,5(0)        R6/32
         SRL   7,27(0)       REST OF THE DIV.
         LR    0,7           R0=NBER OF BITS TO BE MOVED (NBITS<32)
         LR    10,6          R10=NBER OF WORDS OF 32 BITS TO BE MOVED
         L     5,0(3)        R5=1ST WORD X TO BE CHANGED.
*
*      COPIES ONE WORD AT A TIME
LOOP     L     6,0(1)
         L     7,4(1)
         SRDL  6,1(8)
         LR    6,5
         SRL   6,1(9)
         SLDL  6,1(9)
         ST    6,0(3)
         LR    5,7
         LA    3,4(3)
         LA    1,4(1)
         BCT   10,LOOP
*
LASTB    LR    10,4
         AR    10,0
         S     10,ONE
         BNP   FIN
*      THE 'NBITS' OF THE STRING TO BE MOVED ARE LESS THAN 32
         L     6,0(1)
         L     7,4(1)
         SRDL  6,1(8)
         L     10,WORD
         SR    10,0
         SRL   7,0(10)
         SLL   7,0(10)
         LR    6,5
         SRL   6,1(9)
         SLDL  6,1(9)
         AR    0,4
         S     0,ONE
         S     0,WORD
         BM    MINUS
*      THE NBITS ARE TO BE MOVED IN TWO CONSEC. WORDS OF ARRAY X
         ST    6,0(3)
         SR    6,6
         CR    0,6
         BE    FIN
         LA    3,4(3)
         LR    10,0
         SLDL  6,0(10)
         L     5,WORD
         SR    5,10
         B     EQUAL
*      THE NBITS ARE TO BE MOVED IN ONE WORD OF ARRAY X
MINUS    A     0,WORD
         L     5,WORD
         SR    5,0
         SRL   6,0(5)
         LR    10,0
EQUAL    L     7,0(3)
         SLL   7,0(10)
         SLDL  6,0(5)
         ST    6,0(3)
FIN      LM    0,10,24(13)
**       MVI   12(13),X'FF'
         BCR   15,14
NBYTE    DC    F'8'
WORD     DC    F'32'
ONE      DC    F'1'
         END
#endif
