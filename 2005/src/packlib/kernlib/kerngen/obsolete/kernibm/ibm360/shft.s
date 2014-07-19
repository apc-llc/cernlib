*
* $Id: shft.s,v 1.1.1.1 1996/02/15 17:53:16 mclareni Exp $
*
* $Log: shft.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:16  mclareni
* Kernlib
*
*
SHFT     CSECT
*
* CERN PROGLIB#         SHFT            .VERSION KERNIBM  0.1   720321
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
SHFT     AMODE ANY
SHFT     RMODE ANY
#endif
         ENTRY SHFT                    CALL SHFT(A,B)
         BC    15,12(15)
         DC    X'07'
         DC    CL7'SHFT   '
         STM   14,12,12(13)
         LR    11,15
         USING SHFT,11
         ST    13,SA+4
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         L     2,0(1)                  ADR OF A IN REG 2
         L     5,0(2)                  A IN REG 5
         L     4,4(1)
         L     4,0(4)                  B IN REG 4
         SRL   4,24                    SHIFT B(1) TO RIGHT OF REG 5
         SRDL  4,8                     SHIFT BY ONE BYTE
         ST    5,0(2)                  STORE A
         L     13,SA+4
         LM    14,12,12(13)
         BCR   15,14
SA       DS    18F
         END
