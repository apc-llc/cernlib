*
* $Id: vzero370.s,v 1.1.1.1 1996/02/15 17:53:10 mclareni Exp $
*
* $Log: vzero370.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:10  mclareni
* Kernlib
*
*
VZERO    CSECT
*
* CERN PROGLIB# F121    VZERO           .VERSION KERNIBM  1.09  820119
*
*     CALL VZERO  (A,N)
*     CALL VBLANK (A,N)
*
*        ZERO / BLANK ROUTINES. USE THE MVCL INSTR.
*        PROTECTED AGAINST FIELD LENGTH OF ZERO
*        P. M. HEINEN NIJMEGEN SEPT. 73
*        SEPARATED FROM UZERO/UBLANK BY R.MATTHEWS, OCT 81
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
VZERO    AMODE ANY
VZERO    RMODE ANY
#endif
         ENTRY VBLANK
         USING *,15
         STM   2,6,28(13)
         SR    5,5
         B     ZB10
         DROP  15
         USING *,15
VBLANK   STM   2,6,28(13)
         LA    5,X'40'
         DROP  15
ZB10     BALR  6,0
         USING *,6
         LM    2,3,0(1)
         L     4,0(3)  N
         SLA   4,2
         BC    12,ZBR
         SLL   5,24  PADD CHAR / ZERO LENGTH
         LR    3,4  LENGTH
         LR    4,2  ADRESS
         MVCL  2,4
ZBR      LM    2,6,28(13)
         BR    14
         END
#ifdef CERNLIB_IBM360_VZERO
#undef CERNLIB_IBM360_VZERO
#endif
#ifdef CERNLIB_TCGEN_VBLANK
#undef CERNLIB_TCGEN_VBLANK
#endif
#ifdef CERNLIB_TCGEN_VZERO
#undef CERNLIB_TCGEN_VZERO
#endif
