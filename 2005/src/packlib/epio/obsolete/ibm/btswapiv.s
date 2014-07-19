*
* $Id: btswapiv.s,v 1.1.1.1 1996/03/08 15:21:51 mclareni Exp $
*
* $Log: btswapiv.s,v $
* Revision 1.1.1.1  1996/03/08 15:21:51  mclareni
* Epio
*
*
#if defined(CERNLIB_QMIBMVF)
BTSWAP   CSECT
* CERN PROGLIB# FXXX    BTSWAP          .VERSION EPIO     1.64  900509
* ORIG. 13/03/90  C.GUERIN, IBM
*
#if defined(CERNLIB_QMIBMXA)
BTSWAP   AMODE ANY
BTSWAP   RMODE ANY
#endif
**+++++++ ibm nord100/500 and apollo version ++++++++++
**  moves 16 bit words in packed form.
**
** arguments
** iarray     source array
** nbytes     no. of bytes to swap, starting at the leftmost
**            position in iarray. if le 1, no action. if odd            ,
**            nbytes-1 is used instead.
**
         USING *,15
         STM   14,12,12(13)
*****************************************************************
         LM    G5,G6,0(G1)   get addresses
         LR    G8,G5         copy vector address
         LR    G4,G5         copy vector address
         L     G6,0(G6)      get count
         SR    G7,G7         zero G7
         SRDL  G6,2          multiply cunt by 4
**       SRL   G7,30
         LM    G9,G10,MASK0  load masks into G9  G10
VLOOP    EQU   *
         VLVCU G6            set vector loop
         VNS   V1,G9,G5      load v1 & and with x'00FF00FF'
         VSLL  V1,V1,8       shift left  V1 one byte
         VNS   V2,G10,G4     load v2 & and with x'FF00FF00'
         VSRL  V2,V2,8       shift right V2 one byte
         VOR   V3,V1,V2      OR V1 V2 into V3
         VST   V3,G8         store V3 in result area
         BC    2,VLOOP       loop on vector
         LTR   G7,G7         test remaining halfword
         BNZ   REM           if some go to REM
* END SUBROUTINE ************
RETURN2  LM    2,12,28(13)   RETURN
         BR    14
* CONSTANTS *****************************************************
REM      LH    G11,0(G5)     load remaining halfword in G11
         LH    G12,0(G5)     load remaining halfword in G12
         N     G11,MASK3
         SLL   G11,8
         N     G12,MASK4
         SRL   G12,8
         OR    G11,G12
         STH   G11,0(G8)
         B     RETURN2
         DS    0D
MASK0    DC    X'00FF00FF'
MASK1    DC    X'FF00FF00'
MASK3    DC    X'000000FF'
MASK4    DC    X'0000FF00'
G0       EQU   0
G1       EQU   1
G2       EQU   2
G3       EQU   3
G4       EQU   4
G5       EQU   5
G6       EQU   6
G7       EQU   7
G8       EQU   8
G9       EQU   9
G10      EQU   10
G11      EQU   11
G12      EQU   12
G13      EQU   13
G14      EQU   14
G15      EQU   15
F0       EQU   0
F2       EQU   2
F4       EQU   4
F6       EQU   6
V0       EQU   0
V1       EQU   1
V2       EQU   2
V3       EQU   3
V4       EQU   4
         END
#endif
