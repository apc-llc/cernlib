*
* $Id: ubunch.s,v 1.1.1.1 1996/02/15 17:53:05 mclareni Exp $
*
* $Log: ubunch.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:05  mclareni
* Kernlib
*
*
UBUNCH   CSECT               VECTOR VERSION FOR UBUNCH
*
* CERN PROGLIB# M409    UBUNCH          .VERSION KERNIBM  2.30  900508
*
#if defined(CERNLIB_QMIBMXA)
UBUNCH   AMODE 31
UBUNCH   RMODE ANY
#endif
         USING *,15
         STM   14,12,12(13)
*****************************************************************
         LM    G4,G6,0(G1)   get addresses
         L     G6,0(G6)      get count
         C     G6,BEV        compare count with bev
         BL    TZERO         if lower go to TZERO
         SR    G7,G7         zero G7
         SRDL  G6,2          divide count by 4
         LR    G1,G4         copy input vect addr from G4 to G1
         LA    G1,4(G1)      add +4 to G1 for word 2
         LR    G2,G4         copy input vect addr from G4 to G2
         LA    G2,8(G2)      add +8 to G2 for word 3
         LR    G3,G4         copy input vect addr from G4 to G3
         LA    G3,12(G3)     add +12 to G3 for word 4
         LA    G8,4          set stride to 4
         LM    G9,G12,MASK0  load masks in G9 to G12
VLOOP    EQU   *
         VLVCU G6            set vector loop
         VL    V0,G4(G8)     load V0 from G4 addr
         VL    V1,G1(G8)     load V1 from G1 addr
         VL    V2,G2(G8)     load V2 from G2 addr
         VL    V3,G3(G8)     load V3 from G3 addr
         VNQ   V0,G9,V0      and V0 elemt with x'FF000000'
         VSRL  V1,V1,8       shift right V1 for 2nd byte
         VNQ   V1,G10,V1     and V1 elemt with x'00FF0000'
         VSRL  V2,V2,16      shift right V2 for 3rd byte
         VNQ   V2,G11,V2     and V2 elemt with x'0000FF00'
         VSRL  V3,V3,24      shift right V3 for 4th byte
         VNQ   V3,G12,V3     and V3 elemt with x'000000FF'
         VOR   V0,V0,V1      or V1 and V0 in V0 (byte 1 & 2)
         VOR   V2,V3,V2      or V2 and V3 in V2 (byte 3 & 4)
         VOR   V0,V0,V2      or V0 and V2 (byte 1 to 4)
         VST   V0,G5         store V0 (4 bytes) result with G5 addr
         BC    2,VLOOP       go back vector
         SRL   G7,30         shift remainder
         LTR   G7,G7         is any remainder
         BZ    RETURN2       if no return
         L     G12,BLKS      load G12 with blanks
         L     G11,0(G4)     load next input vect elemt
         SRL   G11,24        shift byte to low order position
         ST    G12,0(G5)     store a word of blks
         STC   G11,0(G5)     store one byte from low order
         BCT   G7,NEX1       update count in G7 and test any more
         B     RETURN2       if no more return
NEX1     L     G11,4(G4)     load next input vect elemt
         SRL   G11,24        shift byte to low order position
         STC   G11,1(G5)     store one byte from low order
         BCT   G7,NEX2       update count in G7 and test any more
         B     RETURN2       if no more return
NEX2     L     G11,8(G4)     load next input vect elemt
         SRL   G11,24        shift byte to low order position
         STC   G11,2(G5)     store one byte from low order
         B     RETURN2       if no more return
* END SUBROUTINE ************
RETURN2  LM    2,12,28(13)   RETURN
         BR    14
* SCALAR ********************
TZERO    EQU   *
         C     G6,KZ         compare count with 0
         BC    LE,RETURN2    if le return
SL       L     G1,BLKS       load G1 with blanks
         LR    G9,G5         copy G5 output addr vect in G9
LW       EQU   *
         ST    G1,0(G9)      store G1 blanks in output vect
         ICM   G0,B'1000',0(G4)   get 1 byte from input vect
         STCM  G0,B'1000',0(G5)   put 1 byte in output vect
         BCT   G6,NEXT1      update count and test
         B     EXIT          if no more exit
NEXT1    ICM   G0,B'1000',4(G4)   get 1 byte from input vect
         STCM  G0,B'1000',1(G5)   put 1 byte in output vect
         BCT   G6,NEXT2      update count and test
         B     EXIT          if no more exit
NEXT2    ICM   G0,B'1000',8(G4)   get 1 byte from input vect
         STCM  G0,B'1000',2(G5)   put 1 byte in output vect
         BCT   G6,NEXT3      update count and test
         B     EXIT          if no more exit
NEXT3    ICM   G0,B'1000',12(G4)  get 1 byte from input vect
         STCM  G0,B'1000',3(G5)   put 1 byte from output vect
         LA    G5,4(G5)      add +4 to G5 output vect addr
         LA    G4,16(G4)     add +16 to input vect addr
         LA    G9,4(G9)      add +4 to G9 output vect addr
         BCT   G6,LW         update count and test
EXIT     B     RETURN2       if no more return
* CONSTANTS *****************************************************
         DS    0D
BLKS     DC    X'40404040'
BEV      DC    F'64'
KZ       DC    F'0'
MASK0    DC    X'FF000000'
MASK1    DC    X'00FF0000'
MASK2    DC    X'0000FF00'
MASK3    DC    X'000000FF'
LE       EQU   12
         PRINT OFF
V0       EQU   0
V1       EQU   1
V2       EQU   2
V3       EQU   3
V4       EQU   4
V5       EQU   5
V6       EQU   6
V7       EQU   7
V8       EQU   8
V9       EQU   9
V10      EQU   10
V11      EQU   11
V12      EQU   12
V13      EQU   13
V14      EQU   14
V15      EQU   15
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
         END
*----------------------------------------------------------------
UBLOW    CSECT               VECTOR VERSION FOR UBLOW
*
* CERN PROGLIB# M409    UBLOW           .VERSION KERNIBM  2.28  891004
*
#if defined(CERNLIB_QMIBMXA)
UBLOW    AMODE 31
UBLOW    RMODE ANY
#endif
         USING *,15
         STM   14,12,12(13)
*****************************************************************
         LM    G4,G6,0(G1)   get addresses
         L     G0,BLANKS     load G0 with 3 blanks x'00404040'
         L     G6,0(G6)      get count
         C     G6,BEV        compare count with bev
         BL    TZERO         if lower than bev go to TZERO
         SR    G7,G7         zero G7
         SRDL  G6,1          divide count by 2
         ST    G7,WORD       store remainder
         LR    G8,G5         copy output address G5 in G8
         LA    G8,4(G8)      +4 in G8
         LA    G2,2          set stride=2
VLOOP    EQU   *
         VLVCU G6            set vector loop
         VLH   V0,G4         load halfword from input in V0
         VSLL  V1,V0,24      shift second byte left 24 in V1
         VSRL  V0,V0,8       shift first byte right 8 in V0
         VSLL  V0,V0,24      shift first byte left 24 in V0
         VOQ   V0,G0,V0      set 3 low order bytes to blank
         VOQ   V1,G0,V1      set 3 low order bytes to blank
         VST   V0,G5(G2)     store first byte in output thru G5
         VST   V1,G8(G2)     store 2nd byte in output +4 thru G8
         BC    2,VLOOP       go back vector loop
         TM    WORD,X'80'    test remanider
         BZ    RETURN        if no return
         L     G9,BLANK4     load G9 with 4 blanks x'40404040'
         ST    G9,0(G5)      store in output vector
         IC    G3,0(G4)      get next character
         STC   G3,0(G5)      store in output vector
* END SUBROUTINE ************
RETURN   EQU   *
         LM    2,12,28(13)   RETURN
         BR    14
* NON VECTOR ****************
TZERO    EQU   *
         C     G6,KZ         compare vector count with zero
         BC    LE,RETURN     if le 0 return
ST4      L     G10,0(G4)     get input vector word 1
         L     G11,BLANK4    get 4 blanks
         ST    G11,0(G5)     store in ouput vector
         STCM  G10,B'1000',0(G5)  store first character
         BCT   G6,NEX1       test any more if yes go on
         B     RR            if no more return
NEX1     ST    G11,4(G5)     store 4 blanks in next word
         STCM  G10,B'0100',4(G5)  store next character
         BCT   G6,NEX2       test any more if yes go on
         B     RR            if no more return
NEX2     ST    G11,8(G5)     store 4 blanks in next word
         STCM  G10,B'0010',8(G5)  store next character
         BCT   G6,NEX3       test any more if yes go on
         B     RR            if no more return
NEX3     ST    G11,12(G5)    store 4 blanks in next word
         STCM  G10,B'0001',12(G5) store next character
         LA    G4,4(G4)      add +4 to G4 input vector addr
         LA    G5,16(G5)     add +16 to G5 output vector addr
         BCT   G6,ST4        test count if yes go on scalar
RR       B     RETURN
* CONSTANTS *****************************************************
         DS    0D
BLANK4   DC    X'40404040'
BLANKS   DC    X'00404040'
         DC    X'00404040'
         DC    X'00404040'
         DC    X'00404040'
         DS    0D
BEV      DC    F'40'
KZ       DC    F'0'
WORD     DC    F'0'
LE       EQU   12
         PRINT OFF
V0       EQU   0
V1       EQU   1
V2       EQU   2
V3       EQU   3
V4       EQU   4
V5       EQU   5
V6       EQU   6
V7       EQU   7
V8       EQU   8
V9       EQU   9
V10      EQU   10
V11      EQU   11
V12      EQU   12
V13      EQU   13
V14      EQU   14
V15      EQU   15
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
         END
#ifdef CERNLIB_IBM360_UBUNCH
#undef CERNLIB_IBM360_UBUNCH
#endif
#ifdef CERNLIB_IBM360_UBLOW
#undef CERNLIB_IBM360_UBLOW
#endif
#ifdef CERNLIB_TCGEN_UBUNCH
#undef CERNLIB_TCGEN_UBUNCH
#endif
#ifdef CERNLIB_TCGEN_UBLOW
#undef CERNLIB_TCGEN_UBLOW
#endif
