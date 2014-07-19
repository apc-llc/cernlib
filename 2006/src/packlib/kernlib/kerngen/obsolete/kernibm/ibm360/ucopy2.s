*
* $Id: ucopy2.s,v 1.1.1.1 1996/02/15 17:53:17 mclareni Exp $
*
* $Log: ucopy2.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:17  mclareni
* Kernlib
*
*
UCOPY2   CSECT
*
* CERN PROGLIB# V301    UCOPY2          .VERSION KERNIBM  1.02  780226
*
*        CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
UCOPY2   AMODE ANY
UCOPY2   RMODE ANY
#endif
         USING *,15
         B     BEGIN
         DC    X'6'
         DC    CL6'UCOPY2'
BEGIN    STM   0,12,20(13)
         LM    8,10,0(1)                R8->A,R9->X,R10->N
         L     10,0(10)
         LR    12,8
         SR    12,9
         BZ    END
         BP    COPY
         LR    11,10
         SLL   11,2
         AR    12,11
         BNP   COPY
REVCOPY  SR    11,11                    END OF A OVERLAPS
         LR    12,10                    BEGINNING OF X
         SLL   12,2
         LA    8,0(8,12)
         LA    9,0(9,12)
         SRDA  10,3
         LTR   10,10
         BNP   BYPASS1
         LA    12,32
LOOP1    SR    8,12
         SR    9,12
         LM    0,7,0(8)
         STM   0,7,0(9)
         BCT   10,LOOP1
BYPASS1  SLDA  10,3
         BNP   END
         LR    11,10
         SLL   11,2
         SR    8,11
         SR    9,11
         LM    0,7,0(8)
         BCTR  10,0                     REDUCE R10 BY 1 AS STM GOES
         EX    10,STORE                 FROM R0
         B     END
COPY     SR    11,11                    NO OVERLAP,OR BEGINNING OF A
         SRDA  10,3                     OVERLAPS END OF X
         LA    12,32
         LTR   10,10
         BNP   BYPASS
LOOP     LM    0,7,0(8)
         STM   0,7,0(9)
         AR    8,12
         AR    9,12
         BCT   10,LOOP
BYPASS   SLDA  10,3
         BNP   END
         LM    0,7,0(8)
         BCTR  10,0                     REDUCE R10 BY 1 AS STM GOES
         EX    10,STORE                 FROM R0
END      LM    0,12,20(13)
         MVI   12(13),X'FF'
         SR    15,15
         BR    14
STORE    STM   0,0,0(9)
         END
#ifdef CERNLIB_TCGEN_UCOPY2
#undef CERNLIB_TCGEN_UCOPY2
#endif
