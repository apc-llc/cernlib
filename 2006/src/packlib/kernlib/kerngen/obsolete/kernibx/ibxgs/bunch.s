*
* $Id: bunch.s,v 1.1.1.1 1996/02/15 17:54:36 mclareni Exp $
*
* $Log: bunch.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:36  mclareni
* Kernlib
*
*
BUNCH    CSECT
*
* CERN PROGLIB# Z035    BUNCH           .VERSION KERNIBX  1.01  900523
*
*        CALL BUNCH( INPUT,OUTPUT,'NBYTES',BITS/'BYTE')
*
*  MODIFIED JAN 1982 TO IGNORE CALLS WITH ERRONEOUS PARAMETER VALUES
*  Modified for AIX, Roger Howard, January 1990
*
         USING *,15
         ENTRY _bunch_
_bunch_  STM   2,1,16(13)
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
         LR    9,0         AIX: address of arg1 = INPUT
         LR    8,1         AIX: address of arg2 = OUTPUT
         L     6,0(,3)     AIX: value of arg4 = "bits/byte"
         L     3,0(,2)     AIX: value of arg3 = "nbytes"
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
         SR    10,10      ZERO OUT FIRST OUTPUT REG
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
         LM    2,1,16(13)
         BR    14
         END
#ifdef CERNLIB_TCGEN_BUNCH
#undef CERNLIB_TCGEN_BUNCH
#endif
