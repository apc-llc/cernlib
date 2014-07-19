*
* $Id: ucopyn.s,v 1.1.1.1 1996/02/15 17:53:17 mclareni Exp $
*
* $Log: ucopyn.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:17  mclareni
* Kernlib
*
*
UCOPYN   CSECT
*
* CERN PROGLIB# V301    UCOPYN          .VERSION KERNIBM  1.02  780226
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
UCOPYN   AMODE ANY
UCOPYN   RMODE ANY
#endif
         ENTRY UCOPYN                  CALL UCOPYN (A,B,N)
         BC    15,12(15)
         DC    X'07'
          DC    CL7'UCOPYN '
         STM   14,12,12(13)
         LR    11,15
         USING UCOPYN,11
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
COPY     L     0,0(6,2)
         LCR   0,0
         ST    0,0(6,3)
         BXLE  6,4,COPY
RETURN   DS    0H
         L     13,SA+4
         LM    14,12,12(13)
         BCR   15,14
SA       DS    18F
         END
#ifdef CERNLIB_TCGEN_UCOPYN
#undef CERNLIB_TCGEN_UCOPYN
#endif
