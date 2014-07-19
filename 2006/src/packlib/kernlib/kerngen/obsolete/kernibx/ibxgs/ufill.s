*
* $Id: ufill.s,v 1.1.1.1 1996/02/15 17:54:42 mclareni Exp $
*
* $Log: ufill.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:42  mclareni
* Kernlib
*
*
UFILL    CSECT
*
* CERN PROGLIB# V300    UFILL           .VERSION KERNIBX  1.01  900524
*
*   CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
*   SEPARATED FROM UZERO/UBLANK BY R.MATTHEWS, OCT 81
*   Modified for AIX, Roger Howard, January 1990
*
         USING *,15
         ENTRY _ufill_
_ufill_  STM   2,12,16(13)
         LR    8,0              AIX: address of arg1
         L     9,0(,1)          AIX: value of arg2
         L     10,0(,2)         AIX: value of arg3
         L     2,0(,3)          AIX: value of arg4
         LR    0,2
         LR    1,0
         LR    3,0
         LR    4,0
         LR    5,0
         LR    6,0
         LR    7,0
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
END      LM    2,12,16(13)
         BR    14                 RETURN
MSTOR    STM   0,0,0(8)
         END
#ifdef CERNLIB_TCGEN_UFILL
#undef CERNLIB_TCGEN_UFILL
#endif
