*
* $Id: sbyt.s,v 1.1.1.1 1996/02/15 17:53:16 mclareni Exp $
*
* $Log: sbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:16  mclareni
* Kernlib
*
*
SBYT     CSECT
*
* CERN PROGLIB# M421    SBYT            .VERSION KERNIBM  0.1   741009
*
*        CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
SBYT     AMODE ANY
SBYT     RMODE ANY
#endif
         USING *,15
         B     BEG
         DC    X'4'
         DC    CL4'SBYT'
BEG      STM   0,7,20(13)
         LM    2,5,0(1)                 R2->A,R3->X,R4->LX,R5->NBITS
         L     5,0(5)                   R5=NBITS
         L     4,0(4)                   R4=LX
         L     0,0(2)                   R0=A
         SR    1,1
         SRDL  0,0(5)                   R1 HAS LEFT MOST BITS = BYTE  A
         L     6,0(3)
         AR    4,5
         SRDL  6,63(4)
         LR    0,6
         SLDL  0,0(5)
         SLL   7,0(5)
         LR    1,7
         SR    4,5
         SLDL  0,63(4)
         ST    0,0(3)
         LM    0,7,20(13)
         MVI   12(13),X'FF'
         SR    15,15
         BR    14
         END
#ifdef CERNLIB_TCGEN_SBYT
#undef CERNLIB_TCGEN_SBYT
#endif
