*
* $Id: vzero.s,v 1.1.1.1 1996/02/15 17:54:43 mclareni Exp $
*
* $Log: vzero.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:43  mclareni
* Kernlib
*
*
VZERO    CSECT
*
* CERN PROGLIB# F121    VZERO           .VERSION KERNIBX  1.01  900524
*
*     CALL VZERO  (A,N)
*     CALL VBLANK (A,N)
*
*        ZERO / BLANK ROUTINES. USE THE MVCL INSTR.
*        PROTECTED AGAINST FIELD LENGTH OF ZERO
*        P. M. HEINEN NIJMEGEN SEPT. 73
*        SEPARATED FROM UZERO/UBLANK BY R.MATTHEWS, OCT 81
*        Modified for AIX, Roger Howard, January 1990
*
         USING *,15
         ENTRY _vzero_
         ENTRY _vblank_
_vzero_  STM   2,6,16(13)
         SR    5,5
         B     ZB10
         USING *,15
_vblank_ STM   2,6,16(13)
         LA    5,X'20'        ... use ascii on AIX
         DROP  15
ZB10     BALR  6,0
         USING *,6
         L     4,0(,1)        AIX: value of arg2 = N
         LR    2,0            AIX: address of arg1 = A
         SLA   4,2
         BC    12,ZBR
         SLL   5,24  PADD CHAR / ZERO LENGTH
         LR    3,4  LENGTH
         LR    4,2  ADRESS
         MVCL  2,4
ZBR      LM    2,6,16(13)
         BR    14
         END
#ifdef CERNLIB_TCGEN_VBLANK
#undef CERNLIB_TCGEN_VBLANK
#endif
#ifdef CERNLIB_TCGEN_VZERO
#undef CERNLIB_TCGEN_VZERO
#endif
