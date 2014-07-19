*
* $Id: vfill.s,v 1.1.1.1 1996/02/15 17:54:43 mclareni Exp $
*
* $Log: vfill.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:43  mclareni
* Kernlib
*
*
VFILL    CSECT
*
* CERN PROGLIB# F121    VFILL           .VERSION KERNIBX  1.01  900524
*
*   CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
*   SEPARATED FROM VZERO/VBLANK BY R.MATTHEWS, OCT 81
*   Modified for AIX, Roger Howard, January 1990
*
         USING *,15
         ENTRY _vfill_
_vfill_  STM   2,12,16(13)
         LR    8,0              AIX: address of arg1
         LR    9,1              AIX: address of arg2
         L     2,0(2)           AIX: value of arg3
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
END      LM    2,12,16(13)
         BR    14
MSTOR    STM   0,0,0(8)
         END
#ifdef CERNLIB_TCGEN_VFILL
#undef CERNLIB_TCGEN_VFILL
#endif
