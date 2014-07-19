*
* $Id: ucopy370.s,v 1.1.1.1 1996/02/15 17:53:10 mclareni Exp $
*
* $Log: ucopy370.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:10  mclareni
* Kernlib
*
*
UCOPY    CSECT
*
* CERN PROGLIB# V301    UCOPY           .VERSION KERNIBM  1.03  780715
*
*     CALL UCOPY  (A,B,N)
*     CALL UCOPY2 (A,B,N)
*
*        USES THE MVCL INSTR.
*        MVCL CHECKS ON DESTRUCTIVE OVERLAP OF A AND B
*        ON THAT CONDITION (3), A REVERSE LOOP IS ENTERED.
*        P. M. HEINEN NIJMEGEN SEPT. 73
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
UCOPY    AMODE ANY
UCOPY    RMODE ANY
#endif
         ENTRY UCOPY,UCOPY2
         USING *,15
         STM   2,5,28(13)
         LM    3,5,0(1)
         L     5,0(5)  N
         LPR   5,5
         BC    8,MVR
         SLL   5,2  N
         LR    2,4  B
         LR    4,3  A
         LR    3,5  N
         MVCL  2,4
         BC    1,MV10  DESTR. OVERLAP
MVR      LM    2,5,28(13)
         BR    14
*        REVERSE LOOP / A(N) TO B(N), A(N-1) TO B(N-1) , ETC.
MV10     LA    3,4
         LA    2,0(5,2)  B(N+1)
         LA    4,0(5,4)  A(N+1)
         SRL   5,2  N
MV12     SR    2,3  B(N)
         SR    4,3  A(N)
         MVC   0(4,2),0(4)  A(N) TO B(N)
         BCT   5,MV12
         B     MVR
UCOPY2   EQU   UCOPY
         DROP  15
         END
#ifdef CERNLIB_IBM360_UCOPY
#undef CERNLIB_IBM360_UCOPY
#endif
#ifdef CERNLIB_IBM360_UCOPY2
#undef CERNLIB_IBM360_UCOPY2
#endif
#ifdef CERNLIB_TCGEN_UCOPY
#undef CERNLIB_TCGEN_UCOPY
#endif
#ifdef CERNLIB_TCGEN_UCOPY2
#undef CERNLIB_TCGEN_UCOPY2
#endif
