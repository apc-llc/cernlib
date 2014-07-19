*
* $Id: cmprsb.s,v 1.1.1.1 1996/03/21 17:19:57 mclareni Exp $
*
* $Log: cmprsb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:57  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE CMPRSB (NW,VEC,BIT)                                *
*                                                                   *
*                                                                   *
* Compresses the vector VEC(NW) according to BIT pattern            *
*                                                                   *
* Author:  M.Roethlisberger/IBM                                     *
* Date  :  18-08-89                                                 *
*                                                                   *
*********************************************************************
CMPRSB  CSECT
#if defined(CERNLIB_QMIBMXA)
CMPRSB  AMODE 31
CMPRSB  RMODE ANY
#endif
#if defined(CERNLIB_3090S)||defined(CERNLIB_3090J)
SZ       EQU   257
#endif
#if (!defined(CERNLIB_3090S))&&(!defined(CERNLIB_3090J))
SZ       EQU   129
#endif
         USING *,15
         STM   1,5,24(13)
*GET ADDRESS OF PARM LIST
         LM    G1,G3,0(G1)   GET ADDRESSES
         L     G1,0(0,1)     GET COUNT
         LR    G5,G2
*
* TEST ON VECTOR SIZE IF VS LT "SZ" NOSECTIONING
         LA    G4,SZ
         SR    G4,G1
         BC    2,NOSECT
*
*
* IN R2 R3 ADDRESSES OF VECT AND INDEX
* ADDRESSES OF SUBSEQUENT INDEX SECTION
LOOPV    VLVCU G1
         VLVM  G3
         VLME  V2,G2(0)
         VSTKE V2,G5(0)
         LTR   G1,G1
         BC    2,LOOPV
* CONCLUSION VECTOR
*
         LM    1,5,24(13)
         BR    14
*
* NO SECTIONING
*
*
NOSECT   VLVCA 0(1)
         VLVM  G3
         LR    G5,G2
         VLME  V2,G2(0)
         VSTKE V2,G5(0)
* CONCLUSION NO SECTIONING
*
         LM    1,5,24(13)
         BR    14
*
G1       EQU   1
G2       EQU   2
G3       EQU   3
G4       EQU   4
G5       EQU   5
V2       EQU   2
*
*
         END
#endif
