*
* $Id: getbitib.s,v 1.1.1.1 1996/02/15 17:47:43 mclareni Exp $
*
* $Log: getbitib.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:43  mclareni
* Kernlib
*
*
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_FORTRAN))
      TITLE 'GETBIT/SETBIT - GET OR SET THE VALUE OF A BIT IN AN ARRAY'
GETBIT   CSECT
#if defined(CERNLIB_QMIBMXA)
GETBIT   AMODE ANY
GETBIT   RMODE ANY
#endif
***********************************************************************
*                                                                     *
* CALL GETBIT(I,M,L) GETS THE VALUE OF A SELECTED BIT IN AN ARRAY     *
* CALL SETBIT(I,M,L) SETS THE VALUE OF A SELECTED BIT IN AN ARRAY     *
*                                                                     *
* ARGUMENTS:                                                          *
*    I - A 4-BYTE INTEGER CONTAINING THE NUMBER OF THE BIT            *
*        (STARTING ON THE LEFT WITH 1)                                *
*    M - AN ARRAY CONTAINING THE BIT                                  *
*    L - A 4-BYTE INTEGER WHOSE RIGHTMOST BIT WILL CONTAIN THE VALUE  *
*        FOUND BY GETBIT OR THE VALUE TO BE SET BY SETBIT             *
*                                                                     *
***********************************************************************
         SPACE 5
         USING GETBIT,15      ESTABLISH ADDRESSABILITY
         STM   14,12,12(13)   SAVE THE REGISTER CONTENTS
         SR    11,11          R11 := 0 TO INDICATE ENTRY AT GETBIT
         LA    15,SETBIT      CHANGE TO COMMON BASE ADDRESS
         USING SETBIT,15
         B     CONTINUE       BRANCH AROUND SETBIT ENTRY POINT
         SPACE 2
         ENTRY SETBIT
SETBIT   EQU   *
         STM   14,12,12(13)   SAVE THE REGISTER CONTENTS
         LA    11,1           R11 := 1 TO INDICATE ENTRY AT SETBIT
         SPACE 2
*  CALCULATE THE NUMBER OF THE BIT AND BYTE TO BE PROCESSED
         SPACE 1
CONTINUE EQU   *
         LM    2,4,0(1)       R2 -> I;  R3 -> M;  R4 -> L
         L     6,0(0,2)       R6 := I
         BCTR  6,0            R6 := I-1  (SO BIT NUMBERING STARTS AT 0)
         SRDL  6,3            R6 := BYTE NUMBER; R7 CONTAINS BIT NUMBER
         AR    3,6            R3 -> BYTE TO BE PROCESSED
         SRL   7,29           R7 := NUMBER OF THE BIT TO BE PROCESSED
         L     5,=X'FF7F0080' R5 := MASKS
         SRL   5,0(7)         SHIFT THE MASK TO MATCH THE BIT POSITION
         BCT   11,TESTBIT     BRANCH IF ENTRY WAS AT GETBIT
         SPACE 2
*  SET THE BIT ON OR OFF
         SPACE 1
         TM    3(4),X'01'     IS IT TO BE SET ON OR OFF ?
         BO    SWITCHON       BRANCH IF IT IS TO BE SET ON
         SRL   5,16           GET COMPLEMENT OF MASK TO SWITCH BIT OFF
         EX    5,NI           EXECUTE NI TO SWITCH THE BIT OFF
         B     RETURN         BRANCH TO THE RETURN SEQUENCE
         SPACE 1
SWITCHON EQU   *
         EX    5,OI           EXECUTE OI TO SWITCH THE BIT ON
         B     RETURN         BRANCH TO THE RETURN SEQUENCE
         SPACE 2
*  TEST THE BIT
         SPACE 1
TESTBIT  EQU   *
         XC    0(4,4),0(4)    L := 0
         EX    5,TM           TEST THE REQUESTED BIT
         BNO   RETURN         RETURN IF THE BIT IS OFF
         MVI   3(4),X'01'     L := 1
         SPACE 2
RETURN   EQU   *
         LM    14,12,12(13)   RESTORE THE REGISTER CONTENTS
         BR    14             RETURN TO CALLER
         SPACE 2
NI       NI    0(3),X'00'     NI INSTRUCTION TO SET THE BIT OFF
OI       OI    0(3),X'00'     OI INSTRUCTION TO SET THE BIT ON
TM       TM    0(3),X'00'     TM INSTRUCTION TO TEST THE BIT
         SPACE 2
         END
#endif
