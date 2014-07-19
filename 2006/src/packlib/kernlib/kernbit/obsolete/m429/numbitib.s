*
* $Id: numbitib.s,v 1.1.1.1 1996/02/15 17:47:42 mclareni Exp $
*
* $Log: numbitib.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:42  mclareni
* Kernlib
*
*
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_FORTRAN))
*        FUNCTION NUMBIT(N) COUNTS NUMBER OF ONE-BITS IN ARGUMENT N
*        RESULT IS RETURNED IN REGISTER 0.
*
NUMBIT   CSECT
#if defined(CERNLIB_QMIBMXA)
NUMBIT   AMODE ANY
NUMBIT   RMODE ANY
#endif
         USING NUMBIT,15
         STM   14,12,12(13)            SAVE REGISTERS
         L     1,0(0,1)                GET ADDRESS OF ARGUMENT
         L     1,0(0,1)                GET ARGUMENT ITSELFF
         LA    2,32                    INIT SHIFT COUNTER
         SR    3,3                     INIT RESULT
NUMBLP   LTR   1,1                     SET CONDITION CODE
         BC    8,DONE                  TEST IF WORD EMPTY
         BNM   NUMBSHFT                IF SIGN BIT ZERO GO SHIFT
         LA    3,1(0,3)                IF ONE COUNT UP IN RESULT
NUMBSHFT SLL   1,1                     SHIFT ONE POSITION LEFT
         BCT   2,NUMBLP                GO ROUND TILL THRU
DONE     ST    3,20(0,13)              DONE, OVERWRITE OLD REG0
         LM    14,12,12(13)            GET REGISTERS BACK
         BR    14                      AND RETURN TO CALLER
*
         END
#endif
