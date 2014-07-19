*
* $Id: uzero.s,v 1.1.1.1 1996/02/15 17:54:43 mclareni Exp $
*
* $Log: uzero.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:43  mclareni
* Kernlib
*
*
UZERO    CSECT
*
* CERN PROGLIB# V300    UZERO           .VERSION KERNIBX  1.01  900524
*
*     CALL UZERO  (A,N1,N2)
*     CALL UBLANK (A,N1,N2)
*
*        ZERO / BLANK ROUTINES. USE THE MVCL INSTR.
*        PROTECTED AGAINST FIELD LENGTH OF ZERO
*        P. M. HEINEN NIJMEGEN SEPT. 73
*        SEPARATED FROM VZERO/VBLANK BY R.MATTHEWS, OCT 81
*        Modified for AIX, Roger Howard, January 1990
*
         USING *,15
         ENTRY _uzero_
         ENTRY _ublank_
_uzero_  STM   2,6,16(13)
         SR    5,5
         B     ZB10
         USING *,15
_ublank_ STM   2,6,16(13)
         LA    5,X'20'        ... use ascii on AIX
         DROP  15
ZB10     BALR  6,0
         USING *,6
         L     4,0(,2)        AIX: value of arg3 = N2
         L     3,0(,1)        AIX: value of arg2 = N1
         LR    2,0            AIX: address of arg1 = A
         S     3,=F'1'
         BC    4,ZBR
         SR    4,3
         BC    12,ZBR
         SLA   3,2
         LA    2,0(3,2)       A(N1)
         SLA   4,2
         BC    12,ZBR
         SLL   5,24  PADD CHAR / ZERO LENGTH
         LR    3,4  LENGTH
         LR    4,2  ADRESS
         MVCL  2,4
ZBR      LM    2,6,16(13)
         BR    14
         LTORG
         END
#ifdef CERNLIB_TCGEN_UBLANK
#undef CERNLIB_TCGEN_UBLANK
#endif
#ifdef CERNLIB_TCGEN_UZERO
#undef CERNLIB_TCGEN_UZERO
#endif
