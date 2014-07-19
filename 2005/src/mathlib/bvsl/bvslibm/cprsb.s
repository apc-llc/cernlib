*
* $Id: cprsb.s,v 1.1.1.1 1996/03/21 17:19:59 mclareni Exp $
*
* $Log: cprsb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:59  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE CPRSB   (NW,VEC,BIT)                               *
*                                                                   *
*                                                                   *
* Compresses the vector VEC(NW) according to BIT pattern            *
*                                                                   *
* Author:  M.Roethlisberger/IBM                                     *
* Date  :  01-11-89, Vers 1.01                                      *
*                                                                   *
*********************************************************************
CPRSB    CSECT
#if defined(CERNLIB_QMIBMXA)
CPRSB    AMODE 31
CPRSB    RMODE ANY
#endif
#include "cachesz.inc"
*PRSB    START 0
         USING *,15
         STM   G1,G5,24(13)
         LM    G1,G3,0(G1)   Get addresses of parameter list
         L     G1,0(0,G1)    NW in G1
         LR    G5,G2         Save address of VEC for writing
*
         LA    G4,SZ
         SR    G4,G1         SZ - NW
         BC    GE,NOSECT     When G4.ge.G1
*
LOOPV    VLVCU G1            Set vector loop
         VLVM  G3            Load vector mask register
         VLME  V2,G2(0)      Load matched of VEC
         VSTKE V2,G5(0)      Store compressed on VEC
         BC    GT,LOOPV      go back to loopv
*
         LM    1,5,24(13)
         BR    14
*
*  No Sectioning
*  -------------
NOSECT   VLVCA 0(1)
         VLVM  G3            Load Vector Mask Register
         VLME  V2,G2(0)      Load matched of VEC
         VSTKE V2,G5(0)      Store compressed on VEC
         LM    1,5,24(13)
         BR    14
*
#include "equats.inc"
*
         END
#endif
