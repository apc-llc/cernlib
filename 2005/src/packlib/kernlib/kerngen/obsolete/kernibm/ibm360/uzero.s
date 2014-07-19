*
* $Id: uzero.s,v 1.1.1.1 1996/02/15 17:53:18 mclareni Exp $
*
* $Log: uzero.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:18  mclareni
* Kernlib
*
*
UZERO    CSECT
*
* CERN PROGLIB# V300    UZERO           .VERSION KERNIBM  1.09  820119
*
*        CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
*        SEPARATED FROM UFILL BY R.MATTHEWS, OCT 81
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
UZERO    AMODE ANY
UZERO    RMODE ANY
#endif
         ENTRY UBLANK
         USING *,15
         B     BEG1
         DC    X'5'
         DC    CL5'UZERO'
BEG1     STM   14,12,12(13)
         SR    2,2
         B     DO
         USING *,15
UBLANK   B     BEG2
         DC    X'6'
         DC    CL6'UBLANK'
BEG2     STM   14,12,12(13)
         L     2,=CL4' '
DO       LM    8,10,0(1)
         BALR  12,0
         USING *,12
         LR    0,2
         LR    1,0
         LR    3,0
         LR    4,0
         LR    5,0
         LR    6,0
         LR    7,0
         L     9,0(9)
         L     10,0(10)
         SR    10,9
         BM    END
         BCTR  9,0
         SLL   9,2
         AR    8,9
         LA    9,32
         LA    10,1(10)
         SR    11,11
         SRDA  10,3
         LTR   10,10
         BNP   BYPASS
LOOP     STM   0,7,0(8)
         AR    8,9
         BCT   10,LOOP
BYPASS   SLDA  10,3
         BNP   END
         BCTR  10,0
         EX    10,MSTOR
END      LM    14,12,12(13)
         MVI   12(13),X'FF'       SET RETURN CODE
         SR    15,15
         BR    14                 RETURN
MSTOR    STM   0,0,0(8)
         END
#ifdef CERNLIB_TCGEN_UBLANK
#undef CERNLIB_TCGEN_UBLANK
#endif
#ifdef CERNLIB_TCGEN_UZERO
#undef CERNLIB_TCGEN_UZERO
#endif
