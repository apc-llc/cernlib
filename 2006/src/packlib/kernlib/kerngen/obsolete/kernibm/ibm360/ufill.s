*
* $Id: ufill.s,v 1.1.1.1 1996/02/15 17:53:17 mclareni Exp $
*
* $Log: ufill.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:17  mclareni
* Kernlib
*
*
UFILL    CSECT
*
* CERN PROGLIB# V300    UFILL           .VERSION KERNIBM  1.09  820119
*
*        CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
*        SEPARATED FROM UZERO/UBLANK BY R.MATTHEWS, OCT 81
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
UFILL    AMODE ANY
UFILL    RMODE ANY
#endif
         USING *,15
         B     BEG1
         DC    X'5'
         DC    CL5'UFILL'
BEG1     STM   14,12,12(13)
         L     2,12(1)
         L     2,0(2)
         LM    8,10,0(1)
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
#ifdef CERNLIB_TCGEN_UFILL
#undef CERNLIB_TCGEN_UFILL
#endif
