*
* $Id: blow.s,v 1.1.1.1 1996/02/15 17:53:10 mclareni Exp $
*
* $Log: blow.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:10  mclareni
* Kernlib
*
*
BLOW     CSECT
*
* CERN PROGLIB# Z035    BLOW            .VERSION KERNIBM  1.11  820328
*
*        CALL BLOW (INPUT,OUTPUT,'NBYTES',BITS/'BYTE')
*
*   MODIFIED JAN 1982 TO IGNORE CALLS WITH ERRONEOUS PARAMETER VALUES
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
BLOW     AMODE ANY
BLOW     RMODE ANY
#endif
         USING *,15
         B     ROUNDSA
         DC    X'07'
         DC    CL7'BLOW '
SAVEAREA DS    18F
ROUNDSA  STM   14,12,12(13)
         LR    12,13
         LA    13,SAVEAREA
         ST    13,8(12)
         ST    12,4(,13)
         USING SAVEAREA,13
         DROP  15
*
*            REGISTERS
*      1   INDEX FOR STORE OF OUTPUT
*      2   INCREMENT
*      3   COMPARAND  NR ENTRIES-1 *4
*      4   MASK
*      5   NR BITS LEFT AFTER SHIFT
*      6   NR BITS/'BYTE'
*      7   OFFSET OF INPUT
*      8   ADDRESS OF INPUT
*      9   ADDRESS OF OUTPUT
*      10
*      11  REGS FOR SHIFT
*      12  TEMPORARY REG
*
         LM    8,9,0(1)   GET ADDRESS OF INPUT & OUTPUT ARRAYS
         L     3,8(1)     GET NUMBER OF BYTES
         L     3,0(3)
         L     6,12(1)    LOAD NR OF BITS/'BYTE'
         L     6,0(6)
*
*          TEST FOR VALIDITY OF ARGUMENTS
         LTR   12,3        ZERO NUMBER OF BYTES
         BC    12,END      IGNORE AND RETURN
         LTR   12,6        <1 BITS/'BYTE'
         BC    12,END      IGNORE AND RETURN
         LA    12,32(0)
         SR    12,6        >31 BITS/'BYTE'
         BC    12,END      IGNORE AND RETURN
*
         S     3,=F'1'    SUBTRACT ONE FROM NR OF 'BYTES'
         SLA   3,2(0)     MULTIPLY BY 4
         L     5,=X'FFFFFFFF'
         SR    4,4
         SLDL  4,0(6)     CONSTRUCT MASK
         LA     2,4(0)    SET UP INCREMENT = 4 BYTES = 1 FULL WORD
         LA     1,0(0)    ZERO INDEX TO START WITH
         LA    7,0(0)     ZERO INPUT OFFSET
         L     11,0(7,8)  LOAD FIRST WORD
         LA    5,32(0)    SET UP NUMBER OF BITS THAT CAN BE SHIFTED
LOOP     SLDL  10,0(6)    SHIFT
         SR    5,6        SUBTRACT NUMBER OF BITS SHIFTED
         BC    4,LOAD     BRANCH TO LOAD IF < ZERO
LOOP2    NR    10,4       MASK
         ST    10,0(1,9)  STORE
         BXH   1,2,END    BRANCH OUT IF STORE COMPLETE
         B     LOOP
LOAD     LPR   12,5       LOAD NUMBER OF BITS TO SHIFT BACK
         SRDL  10,0(12)   SHIFT BACK
         LA    7,4(7)     LOAD NEW INPUT OFFSET
         L     11,0(7,8)  LOAD NEW 11
         A     5,=F'32'
         SLDL  10,0(12)   SHIFT AGAIN
         B     LOOP2
END      L     13,4(,13)
         LM    2,12,28(13)
         L     14,12(,13)
         MVI   12(13),X'FF'
         BR    14
         END
#ifdef CERNLIB_TCGEN_BLOW
#undef CERNLIB_TCGEN_BLOW
#endif
