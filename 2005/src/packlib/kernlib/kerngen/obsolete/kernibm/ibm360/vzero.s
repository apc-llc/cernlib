*
* $Id: vzero.s,v 1.1.1.1 1996/02/15 17:53:18 mclareni Exp $
*
* $Log: vzero.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:18  mclareni
* Kernlib
*
*
VZERO    CSECT
*
* CERN PROGLIB# F121    VZERO           .VERSION KERNIBM  1.09  820119
*
*        CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
*        SEPARATED FROM VZERO/VBLANK BY R.MATTHEWS, OCT 81
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
VZERO    AMODE ANY
VZERO    RMODE ANY
#endif
         ENTRY VBLANK
         USING *,15
         B     BEG1
         DC    X'5'
         DC    CL5'VZERO'
BEG1     STM   14,12,12(13)
         SR    2,2
         B     DO
         USING *,15
VBLANK   B     BEG2
         DC    X'6'
         DC    CL6'VBLANK'
BEG2     STM   14,12,12(13)
         L     2,=CL4' '
DO       LM    8,9,0(1)
         BALR  12,0
         USING *,12
         LR    0,2
         LR    1,0
         LR    3,0
         LR    4,0
         LR    5,0
         LR    6,0
         LR    7,0
         L     10,0(9)
         LTR   10,10
         BNP   END
         SR    11,11
         SRDA  10,3
         LTR   10,10
         BNP   BYPASS
LOOP     STM   0,7,0(8)
         LA    8,32(8)
         BCT   10,LOOP
BYPASS   SLDA  10,3
         BNP   END
         BCTR  10,0
         EX    10,MSTOR
END      LM    14,12,12(13)
         MVI   12(13),X'FF'
         SR    15,15
         BR    14
MSTOR    STM   0,0,0(8)
         END
#ifdef CERNLIB_TCGEN_VBLANK
#undef CERNLIB_TCGEN_VBLANK
#endif
#ifdef CERNLIB_TCGEN_VZERO
#undef CERNLIB_TCGEN_VZERO
#endif
