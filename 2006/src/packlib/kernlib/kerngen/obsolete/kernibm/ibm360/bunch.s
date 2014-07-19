*
* $Id: bunch.s,v 1.1.1.1 1996/02/15 17:53:11 mclareni Exp $
*
* $Log: bunch.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:11  mclareni
* Kernlib
*
*
BUNCH    CSECT
*
* CERN PROGLIB# Z035    BUNCH           .VERSION KERNIBM  1.11  820328
*
*        CALL BUNCH( INPUT,OUTPUT,'NBYTES',BITS/'BYTE')
*
*  MODIFIED JAN 1982 TO IGNORE CALLS WITH ERRONEOUS PARAMETER VALUES
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
BUNCH    AMODE ANY
BUNCH    RMODE ANY
#endif
         USING *,15
         B     ROUNDSA
         DC    X'07'
         DC    CL7'BUNCH '
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
*      1   INDEX FOR FETCH OF INPUT
*      2   INCREMENT
*      3   COMPARAND  NR ENTRIES-1 *4
*      5   NR BITS LEFT AFTER SHIFT
*      6   NR BITS/'BYTE'
*      7   OFFSET OF OUTPUT
*      8   ADDRESS OF OUTPUT
*      9   ADDRESS OF INPUT
*      10
*      11  REGS FOR SHIFT
*      12  TEMPORARY REG
*
         L     9,0(1)   GET ADDRESS OF OUTPUT & INPUT ARRAYS
         L     8,4(1)
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
         SR    12,6         >31 BITS/'BYTE'
         BC    12,END       IGNORE AND RETURN
*
         S     3,=F'1'    SUBTRACT ONE FROM NR OF 'BYTES'
         SLA   3,2(0)     MULTIPLY BY 4
         LA     2,4(0)    SET UP INCREMENT = 4 BYTES = 1 FULL WORD
         LA     1,0(0)    ZERO INDEX TO START WITH
         LA    7,0(0)     ZERO OUTPUT OFFSET
         LA    5,32(0)    SET UP NUMBER OF BITS THAT CAN BE SHIFTED
         SR    10,10    ZERO OUT FIRST UTPUT REG
LOOP     L     11,0(1,9)  LOAD A 'BYTE'
         LA    12,32(0)   FIGURE OUT HOW MUCH TO SHIFT TO GET RID
         SR    12,6       OF LEADING ZEROES IN 'BYTE'
         SLL   11,0(12)   SHIFT
         SR    5,6        CAN BE SHIFTED WITHOUT SPILL
         BC    4,STORE    IF NOT GO TO STORE
         SLDL  10,0(6)    SHIFT INTO REG 10
LOOP2    BXH   1,2,END    TEST IF ALL DONE
         B     LOOP
STORE    LPR   14,5       ABS OF SPILL
         LR    12,6
         SR    12,14      HOW MANY CAN BE SHIFTED WITHOUT SPILL?
         SLDL  10,0(12)   SHIFT
         ST    10,0(7,8)  STORE PACKED 'BYTES'
         LA    7,4(7)     INCREMENT OFFSET FOR STORE
         SR    10,10      ZERO OUT 10
         SLDL  10,0(14)    SHIFT REST
         A     5,=F'32'   CORRECT NUMBER OF BITS THAT CAN BE SHIFTED
         B     LOOP2      WITHOUT SPILL
END      SLDL  10,0(5)    SHIFT TO DELETE REMAINING ZEROES
         ST    10,0(7,8)  STORE LAST REG 10
         L     13,4(,13)
         LM    2,12,28(13)
         L     14,12(,13)
         MVI   12(13),X'FF'
         BR    14
         END
#ifdef CERNLIB_TCGEN_BUNCH
#undef CERNLIB_TCGEN_BUNCH
#endif
