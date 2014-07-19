*
* $Id: uzero370.s,v 1.1.1.1 1996/02/15 17:53:10 mclareni Exp $
*
* $Log: uzero370.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:10  mclareni
* Kernlib
*
*
UZERO    CSECT
*
* CERN PROGLIB# V300    UZERO           .VERSION KERNIBM  1.09  820119
*
*     CALL UZERO  (A,N1,N2)
*     CALL UBLANK (A,N1,N2)
*
*        ZERO / BLANK ROUTINES. USE THE MVCL INSTR.
*        PROTECTED AGAINST FIELD LENGTH OF ZERO
*        P. M. HEINEN NIJMEGEN SEPT. 73
*        SEPARATED FROM VZERO/VBLANK BY R.MATTHEWS, OCT 81
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
UZERO    AMODE ANY
UZERO    RMODE ANY
#endif
         ENTRY UBLANK
         USING *,15
         STM   2,6,28(13)
         SR    5,5
         B     ZB10
         DROP  15
         USING *,15
UBLANK   STM   2,6,28(13)
         LA    5,X'40'
         DROP  15
ZB10     BALR  6,0
         USING *,6
         LM    2,4,0(1)
         L     4,0(4)  N2
         L     3,0(3)  N1
         S     3,=F'1'
         BC    4,ZBR
         SR    4,3
         BC    12,ZBR
         SLA   3,2
         LA    2,0(3,2)  A(N1)
         SLA   4,2
         BC    12,ZBR
         SLL   5,24  PADD CHAR / ZERO LENGTH
         LR    3,4  LENGTH
         LR    4,2  ADRESS
         MVCL  2,4
ZBR      LM    2,6,28(13)
         BR    14
         LTORG
         END
#ifdef CERNLIB_IBM360_UZERO
#undef CERNLIB_IBM360_UZERO
#endif
#ifdef CERNLIB_TCGEN_UBLANK
#undef CERNLIB_TCGEN_UBLANK
#endif
#ifdef CERNLIB_TCGEN_UZERO
#undef CERNLIB_TCGEN_UZERO
#endif
