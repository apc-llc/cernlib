*
* $Id: locatf.s,v 1.1.1.1 1996/02/15 17:48:33 mclareni Exp $
*
* $Log: locatf.s,v $
* Revision 1.1.1.1  1996/02/15 17:48:33  mclareni
* Kernlib
*
*
#if defined(CERNLIB_QMIBMVF)
* ** last mod 2 Oct 1990 15:06:51
LOCATF   CSECT               vector version of locatf
*                            tuned for 3090E cern
*                            section size 128
#if defined(CERNLIB_QMIBMXA)
LOCATF   AMODE 31
LOCATF   RMODE ANY
#endif
*****************************************************************
         PRINT NOGEN         vector version of locatf
         USING *,15          tuned for 3090E CERN
         STM   14,12,12(13)  (section size 128)
*--------------------------
KSECT    EQU   128
*****************************************************************
         LM    G4,G6,0(G1)   get addresses
         LE    F0,0(G6)      get object
         L     G5,0(G5)      get length
         SR    G0,G0         zero g0 for work
*--------------------------
         C     G5,K64       comp length with 64
         BL    L64          if lt 64 go to L64
*====================
         S     G5,K4096      comp length with 4096
         BM    P32           if lt 4096 go to p32
         S     G5,K4096      comp length with 8192
         BM    P64           if lt 8192 go to p64
         B     PSCAV         else go to pscav
*====================
P32      EQU   *             vector length lt 4096
         A     G5,K4064      reset to length-32
         LA    G8,32(G5)     copy G5 in G8 (length)
         LR    G11,G4        copy array address
*====================
         LA    G4,KSECT(G4)  add 128 (32 words) to begin addr
         LA    G7,32         set G7 to stride (32)
*====================
         SR    G6,G6         zero G6 for work
******************           vector loop
         SRL   G8,5          divide length by 32
VLOOP    EQU   *             vector loop on full vector
         VLVCU G8            with stride = 32
         VCES  LE,F0,G4(G7)  search object
         VCZVM G6            count left zero in vmr
         BC    8,NF          if none go to nf
         SLL   G6,5          mult sum by 32 (stride)
         B     NEXTK         go to seq search
*====================
NF       EQU   *             not found in sect
         LTR   G8,G8         test vector length
         BC    2,VLOOP       if some more loop
         LR    G6,G5         set g6 to length-32
NEXTK    EQU   *             seq search with length = 32
         LR    G7,G6         copy k-1
         SLL   G6,2          (k-1)*4
         AR    G11,G6        array(k) address
         LR    G3,G11        copy array address
******   SR    G0,G0         zero g0 for work
******************           seq search
         VLVCA 32(0)         with length=32
         VCES  LE,F0,G3      search object
         VCZVM G0            count left zeros (n-1)
         LR    G2,G0         copy count in G2
         SLL   G2,2          mult by 4 (convert to addr)
         AR    G0,G7         add displ
         CE    F0,0(G11,G2)  comp object for equal
         BE    EQUAL         if eq branch
         LCR   G0,G0         set (n-1) = -(n-1)
         LM    2,12,28(13)
         BR    14
* END SUBROUTINE *********** seq search
EQUAL    A     G0,K1         set value to n
         LM    2,12,28(13)
         BR    14
* END SUBROUTINE *********** seq search
* no sectionning ************
L64      EQU   *             no sect length le 32
         LR    G3,G4         get array addr
         VLVCU G5            set vector length (le.64)
         VCES  LE,F0,G3      search onject
         VCZVM G0            count left zeros (n-1)
         LR    G2,G0         copy count in G2
         SLL   G2,2          mult by 4 (convert to addr)
         CE    F0,0(G4,G2)   comp object for equal
         BE    EQL64         if eq branch
         LCR   G0,G0         if ne set (n-1)= -(n-1)
         LM    2,12,28(13)   reset
         BR    14            return
* END SUBROUTINE ***********
EQL64    A     G0,K1         set value to n
         LM    2,12,28(13)   reset
         BR    14            return
* END SUBROUTINE ***********
*****************************************************************
PSCAV    EQU   *
         A     G5,K8192      RESET G5 VALUE (LENGTH)
****     SR    G0,G0         zero g0 for work
         SR    G1,G1         nbel=0
         LA    G5,1(G5)      nab = length + 1
DIX      EQU   *
         LR    G6,G5         copy nab
         SR    G6,G1         g7 = (nab-nbel)
         C     G6,K128
         BNH   VECT          then go vector (one sect max)
         LR    G12,G5        copy nab in g12
         AR    G12,G1        g12 = (nab+nbel)
         SRL   G12,1         g12 = (nab+nbel)/2
         LR    G11,G12       copy mid in g11
         BCTR  G12,0         mid-1
         SLL   G12,2         (mid-1)*4 for addressing
         CE    F0,0(G4,G12)  if(object-array(mid)) 100,180,140
         BE    EQU           180
         BL    MINUS         100
PLUS     EQU   *             140
         LR    G1,G11        nbel=mid
         B     DIX           go to dix
MINUS    EQU   *             100
         LR    G5,G11        nab=mid
         B     DIX           go to dix
EQU      LR    G0,G11        locata=mid     180
#include "exitg.inc"
VECT     LR    G8,G1         get nbel
         SLL   G8,2          g8 = (nbel+1)*4
         AR    G8,G4         g8=array(nbel+1)
         LR    G9,G8         g9=g8=array(nbel+1)
         VLVCU G6            vect length= (nab-nbel)
         VCES  LE,F0,G8      search object
         VCZVM G0            get loc
         BC    8,NFS
         LR    G2,G0         copy in g2
         SLL   G2,2          mult by 4
         CE    F0,0(G9,G2)   comp object array(loc)
         BE    EQUV
         AR    G0,G1         locata=loc+nbel
         LCR   G0,G0         locata= - locata
#include "exitg.inc"
EQUV     AR    G0,G1
         A     G0,K1         locata=loc+nbel
#include "exitg.inc"
NFS      S     G0,K1         LOCATA= - LOCATA
         AR    G0,G1         locata=loc+nbel
         LCR   G0,G0         locata= - locata
#include "exitg.inc"
*****************************************************************
*====================
P64      EQU   *
         A     G5,K8128      reset G5 to length-64
         LA    G8,64(G5)     copy length in G8
         LR    G11,G4        copy array address
*====================
         LA    G4,256(G4)    add 256 (64 words) to begin addr
         LA    G7,64         set G7 to stride (64)
*====================
         SR    G6,G6         zero G6 for work
******************           vector loop
         SRL   G8,6          divide length by 64
VL64     EQU   *             vector loop on full vector
         VLVCU G8            with stride = 64
         VCES  LE,F0,G4(G7)  search object
         VCZVM G6            count left zero in vmr
         BC    8,NF64
         SLL   G6,6          mult sum by 64 (stride)
         B     SEQ64
*====================
NF64     EQU   *             not found in sect
         LTR   G8,G8         test vector count
         BC    2,VL64        if some more loop
         LR    G6,G5         set g6 to length-64
SEQ64    EQU   *             seq search with length = 64
         LR    G7,G6         copy k-1
         SLL   G6,2          (k-1)*4
         AR    G11,G6        array(k) address
         LR    G3,G11        copy array address
****     SR    G0,G0         zero g0 for work
******************           seq search
         VLVCA 64(0)         with length=64
         VCES  LE,F0,G3      search object
         VCZVM G0            count left zeros (n-1)
         LR    G2,G0         copy count in G2
         SLL   G2,2          mult by 4 (convert to addr)
         AR    G0,G7         add displ
         CE    F0,0(G11,G2)  comp object for equal
         BE    EQ64          if eq branch
         LCR   G0,G0         set (n-1) = -(n-1)
         LM    2,12,28(13)
         BR    14
* END SUBROUTINE *********** seq search
EQ64     A     G0,K1         set value to n
         LM    2,12,28(13)
         BR    14
* END SUBROUTINE *********** seq search
*====================
         DS    0D
K1       DC    F'1'
K32      DC    F'32'
K48      DC    F'48'
K64      DC    F'64'
K128     DC    F'128'
K2048    DC    F'2048'
K3072    DC    F'3072'
K4064    DC    F'4064'
K4096    DC    F'4096'
K6144    DC    F'6144'
K8128    DC    F'8128'
K8192    DC    F'8192'
#include "equats.inc"
         END
#endif
