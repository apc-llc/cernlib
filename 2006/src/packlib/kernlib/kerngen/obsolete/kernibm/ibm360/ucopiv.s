*
* $Id: ucopiv.s,v 1.1.1.1 1996/02/15 17:53:17 mclareni Exp $
*
* $Log: ucopiv.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:17  mclareni
* Kernlib
*
*
UCOPIV   CSECT
*
* CERN PROGLIB# V301    UCOPIV          .VERSION KERNIBM  0.1   720321
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
UCOPIV   AMODE ANY
UCOPIV   RMODE ANY
#endif
         ENTRY UCOPIV                  CALL UCOPIV(A,B,N)
         BC    15,12(15)
         DC    X'07'
         DC    CL7'UCOPIV '
         STM   14,12,12(13)
         LR    11,15
         USING UCOPIV,11
         ST    13,SA+4
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         LM    2,4,0(1)
         L     5,0(4)
         LTR   5,5
         BC    8,RETURN
         LA    4,4(0)
         SLA   5,2
         SR    5,4
         SR    6,6
         LA    7,COPY
COPY     LE    0,0(6,2)
         STE   0,0(5,3)
         LA    6,4(6)
         SR    5,4
         BCR   10,7
RETURN   DS    0H
         L     13,SA+4
         LM    14,12,12(13)
         BCR   15,14
SA       DS    18F
         END
#ifdef CERNLIB_TCGEN_UCOPIV
#undef CERNLIB_TCGEN_UCOPIV
#endif
