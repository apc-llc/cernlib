*
* $Id: locf.s,v 1.1.1.1 1996/02/15 17:53:14 mclareni Exp $
*
* $Log: locf.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:14  mclareni
* Kernlib
*
*
LOCF     CSECT
*
* CERN PROGLIB#         LOCF            .VERSION KERNIBM  1.09  820119
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
LOCF     AMODE ANY
LOCF     RMODE ANY
#endif
         L     15,0(1)
         LA    15,0(15)
         LR    0,15          . R0 = BYTE - ADR
         SRA   0,2           . R0 = WORD - ADR
         BCR   15,14
         END
#ifdef CERNLIB_TCGEN_LOCF
#undef CERNLIB_TCGEN_LOCF
#endif
