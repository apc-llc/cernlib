*
* $Id: mxmad.s,v 1.1.1.1 1996/02/15 17:53:15 mclareni Exp $
*
* $Log: mxmad.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:15  mclareni
* Kernlib
*
*
MXMAD    CSECT
*
* CERN PROGLIB# F110    MXMAD           .VERSION KERNIBM  2.11  850320
*
* MXMAD  WITH ENTRIES MXMAD,  MXMAD1, MXMAD2, MXMAD3, MXMPY, MXMPY1
* MXMPY2 WITH ENTRIES MXMPY2, MXMPY3, MXMUB, MXMUB1, MXMUB2, MXMUB3
*
* STANDARD PROLOGUE, ENTRY CODE AND EPILOGUE ADDED.
* R. MATTHEWS, CERN/DD, FEBRUARY 1985.
*
********************************************************************
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
MXMAD    AMODE ANY
MXMAD    RMODE ANY
#endif
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
         PRINT NOGEN
*
*  P R O L O G U E
*
         USING MXMAD,R15
         B     SAVEREGS
         DC    AL1(7),CL7'MXMAD'     IDENTIFIER
         DC    C'2.00'               VERSION.MODIFICATION LEVEL
SAVEREGS STM   R14,R12,12(R13)
         LR    R4,R13
         CNOP  0,4
         BAL   R13,AROUNDSA
SAVEAREA DC    A(EPILOGUE),17F'0'
         DROP  R15
         USING SAVEAREA,R13
AROUNDSA ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
*
         LA    4,0(0)
         LA    5,0(0)
         B     TESTIK
*
*  E N T R Y   P O I N T   M X M A D 1
*
         ENTRY MXMAD1                  CALL MXMAD1(A,B,C,I,J,K)
         USING MXMAD1,R15
         CNOP  0,4
MXMAD1   B     EP0102
         DC    AL1(7),CL7'MXMAD1'
EP0101   DC    A(SAVEAREA)
EP0102   STM   R14,R12,12(R13)
         LR    R4,R13
         L     R13,EP0101
         ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
         DROP  R15
*
         LA    4,0(0)
         LA    5,1(0)
         B     TESTIK
*
*  E N T R Y   P O I N T   M X M A D 2
*
         ENTRY MXMAD2                  CALL MXMAD2(A,B,C,I,J,K)
         USING MXMAD2,R15
         CNOP  0,4
MXMAD2   B     EP0202
         DC    AL1(7),CL7'MXMAD2'
EP0201   DC    A(SAVEAREA)
EP0202   STM   R14,R12,12(R13)
         LR    R4,R13
         L     R13,EP0201
         ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
         DROP  R15
*
         LA    4,1(0)
         LA    5,0(0)
         B     TESTIK
*
*  E N T R Y   P O I N T   M X M A D 3
*
         ENTRY MXMAD3                  CALL MXMAD3(A,B,C,I,J,K)
         USING MXMAD3,R15
         CNOP  0,4
MXMAD3   B     EP0302
         DC    AL1(7),CL7'MXMAD3'
EP0301   DC    A(SAVEAREA)
EP0302   STM   R14,R12,12(R13)
         LR    R4,R13
         L     R13,EP0301
         ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
         DROP  R15
*
         LA    4,1(0)
         LA    5,1(0)
         B     TESTIK
*
*  E N T R Y   P O I N T   M X M P Y
*
         ENTRY MXMPY                   CALL MXMPY(A,B,C,I,J,K)
         USING MXMPY,R15
         CNOP  0,4
MXMPY    B     EP0402
         DC    AL1(7),CL7'MXMPY'
EP0401   DC    A(SAVEAREA)
EP0402   STM   R14,R12,12(R13)
         LR    R4,R13
         L     R13,EP0401
         ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
         DROP  R15
*
         LA    4,0(0)
         LA    5,0(0)
         B     CZERO
*
*  E N T R Y   P O I N T   M X M P Y 1
*
         ENTRY MXMPY1                  CALL MXMPY1(A,B,C,I,J,K)
         USING MXMPY1,R15
         CNOP  0,4
MXMPY1   B     EP0502
         DC    AL1(7),CL7'MXMPY1'
EP0501   DC    A(SAVEAREA)
EP0502   STM   R14,R12,12(R13)
         LR    R4,R13
         L     R13,EP0501
         ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
         DROP  R15
*
         LA    4,0(0)
         LA    5,1(0)
*
CZERO    L     10,12(1)                I IN REG. 10
         L     10,0(10)                I IM REG. 10
         L     15,20(1)
         L     15,0(15)                K IN REG. 15
         MR    14,10                   I*K IN REG. 15
         LTR   15,15
         BC    9,RETURN1               IF I=0 OR K=0 RETURN
         SLL   15,2(0)
         S     15,FIXVIER              I*K-1 IN REG. 15
         L     6,8(1)                  C
         LA    7,0(0)                  IC = 1 IN REG. 7
         SDR   0,0                     CLEAR FL.REG. 0
LOOPIC   STE   0,0(7,6)                C(IC) = 0.
         LA    7,4(7)                  IC = IC + 1
         CR    7,15
         BC    12,LOOPIC
         B     TESTJ
TESTIK   L     10,12(1)
         L     10,0(10)                I IM REG. 10
         L     15,20(1)
         L     15,0(15)                K IN REG. 15
         MR    14,10                   I*K IN REG. 15
         LTR   15,15
         BC    9,RETURN1               IF I=0 OR K=0 RETURN
TESTJ    L     3,16(1)
         L     3,0(3)                  J IN REG. 3
         LTR   3,3
         BC    9,RETURN1
STOSA    L     15,12(1)            I IN REG. 15
          L    15,0(15)
         ST    15,I                SAVE I
         L     15,20(1)            K IN REG. 15
         L     15,0(15)
         ST    15,K                SAVE K
         LTR   4,4
         BC    9,DREI                  TEST ENTRY
         L     10,I
         SLL   10,2(0)                 IIA = 4*I IN REG. 10
         MVC   IOA,FIXVIER             IOA = 4*1
         SLL   3,2(0)                  4*J IN REG. 3
         B     SIEBEN
DREI     LA    10,4(0)                 IIA = 1*4
         L     3,16(1)                 J
         L     3,0(3)
         SLL   3,2(0)                  4*J IN REG. 3
         ST    3,IOA                   IOA = 4*J
SIEBEN   LTR   5,5
         BC    9,NEUN                  TEST ENTRY
         LA    11,4(0)                 IIB = 1*4 IN REG. 11
         ST    3,IOB                   IOB = 4*J
         B     DREIZEHN
NEUN     LR    11,15
         SLL   11,2(0)                 IIB = 4*K IN REG. 11
         MVC   IOB,FIXVIER             IOB = 1*4
DREIZEHN MVC   IA,FIXNULL              IA = 0*1
         LA    7,0(0)                  IC = 0*1 IN REG. 7
         L     4,0(1)                  A
         L     5,4(1)                  B
         L     6,8(1)                  C
         LA    14,1(0)                 L = 1 IN REG. 14
LOOPL    L     15,K
         LA    0,0(0)              IB=1 IN REG. 0
         LA    12,1(0)                 M = 1 IN REG. 12
LOOPM    L     8,IA                    JA = IA IN REG. 8
         LR    9,0                     JB = IB IN REG. 9
         LA    1,4(0)                  N = 1*4 IN REG. 1
         SDR   0,0                     CLEAR FL.REG. 0
         SDR   4,4                     CLEAR FL.REG. 4
LOOPN    LE    2,0(8,4)                A(JA) IN FL.REG. 2
         ME    2,0(9,5)
         ADR   4,2
         AR    8,10                    JA = JA + IIA
         AR    9,11                    JB = JB + IIB
         LA    1,4(1)                  INCREASE N
         CR    1,3
         BC    12,LOOPN
         LE    0,0(7,6)
         ADR   0,4
         STE   0,0(7,6)                C = C + A*B
         A     0,IOB                   IB = IB + IOB
ZWEIDREI LA    7,4(7)                  IC = IC + 1
         LA    12,1(12)                INCREASE M
         CR    12,15
         BC    12,LOOPM
         L     1,IA
         A     1,IOA
         ST    1,IA                    IA = IA + IOA
         LA    14,1(14)                INCREASE L
         L     15,I
         CR    14,15
         BC    12,LOOPL
*
*  E P I L O G U E
*
EPILOGUE EQU   *
RETURN1  EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         MVI   12(R13),X'FF'
         BR    R14
*
*  D A T A   A R E A S
*
I        DS    1F
K        DS    1F
FIXVIER  DC    F'4'
FIXNULL  DC    F'0'
IA       DS    1F
IOA      DS    1F
IOB      DS    1F
*
         END
*      ========================================
MXMPY2   CSECT
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
MXMPY2   AMODE ANY
MXMPY2   RMODE ANY
#endif
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
         PRINT NOGEN
*
*  P R O L O G U E
*
         USING MXMPY2,R15
         B     SAVEREGS
         DC    AL1(7),CL7'MXMPY2'    IDENTIFIER
         DC    C'2.00'               VERSION.MODIFICATION LEVEL
SAVEREGS STM   R14,R12,12(R13)
         LR    R4,R13
         CNOP  0,4
         BAL   R13,AROUNDSA
SAVEAREA DC    A(EPILOGUE),17F'0'
         DROP  R15
         USING SAVEAREA,R13
AROUNDSA ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
*
         LA    4,1(0)
         LA    5,0(0)
         B     CZERO
*
*  E N T R Y   P O I N T   M X M P Y 3
*
         ENTRY MXMPY3                  CALL MXMPY3(A,B,C,I,J,K)
         USING MXMPY3,R15
         CNOP  0,4
MXMPY3   B     EP0102
         DC    AL1(7),CL7'MXMPY3'
EP0101   DC    A(SAVEAREA)
EP0102   STM   R14,R12,12(R13)
         LR    R4,R13
         L     R13,EP0101
         ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
         DROP  R15
*
         LA    4,1(0)
         LA    5,1(0)
         B     CZERO
*
*  E N T R Y   P O I N T   M X M U B
*
         ENTRY MXMUB                   CALL MXMUB(A,B,C,I,J,K)
         USING MXMUB,R15
         CNOP  0,4
MXMUB    B     EP0202
         DC    AL1(7),CL7'MXMUB'
EP0201   DC    A(SAVEAREA)
EP0202   STM   R14,R12,12(R13)
         LR    R4,R13
         L     R13,EP0201
         ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
         DROP  R15
*
         LA    4,0(0)
         LA    5,0(0)
         B     CMINUS
*
*  E N T R Y   P O I N T   M X M U B 1
*
         ENTRY MXMUB1                  CALL MXMUB1(A,B,C,I,J,K)
         USING MXMUB1,R15
         CNOP  0,4
MXMUB1   B     EP0302
         DC    AL1(7),CL7'MXMUB1'
EP0301   DC    A(SAVEAREA)
EP0302   STM   R14,R12,12(R13)
         LR    R4,R13
         L     R13,EP0301
         ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
         DROP  R15
*
         LA    4,0(0)
         LA    5,1(0)
         B     CMINUS
*
*  E N T R Y   P O I N T   M X M U B 2
*
         ENTRY MXMUB2                  CALL MXMUB2(A,B,C,I,J,K)
         USING MXMUB2,R15
         CNOP  0,4
MXMUB2   B     EP0402
         DC    AL1(7),CL7'MXMUB2'
EP0401   DC    A(SAVEAREA)
EP0402   STM   R14,R12,12(R13)
         LR    R4,R13
         L     R13,EP0401
         ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
         DROP  R15
*
         LA    4,1(0)
         LA    5,0(0)
         B     CMINUS
*
*  E N T R Y   P O I N T   M X M U B 3
*
         ENTRY MXMUB3                  CALL MXMUB3(A,B,C,I,J,K)
         USING MXMUB3,R15
         CNOP  0,4
MXMUB3   B     EP0502
         DC    AL1(7),CL7'MXMUB3'
EP0501   DC    A(SAVEAREA)
EP0502   STM   R14,R12,12(R13)
         LR    R4,R13
         L     R13,EP0501
         ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
         DROP  R15
*
         LA    4,1(0)
         LA    5,1(0)
*
CMINUS   L     10,12(1)
         L     10,0(10)                I IM REG. 10
         L     15,20(1)
         L     15,0(15)
         MR    14,10                   I*K IN REG. 15
         LTR   15,15
         BC    9,RETURN1               IF I=0 OR K=0 RETURN
         SLL   15,2(0)
         S     15,FIXVIER              I*K-1 IN REG. 15
         L     6,8(1)
         LA    7,0(0)                  IC = 1 IN REG. 7
LOOPIC1  SER   0,0
         SE    0,0(7,6)
         STE   0,0(7,6)
         LA    7,4(7)
         CR    7,15
         BC    12,LOOPIC1
         B     TESTJ
CZERO    L     10,12(1)                I IN REG. 10
         L     10,0(10)                I IM REG. 10
         L     15,20(1)
         L     15,0(15)                K IN REG. 15
         MR    14,10                   I*K IN REG. 15
         LTR   15,15
         BC    9,RETURN1               IF I=0 OR K=0 RETURN
         SLL   15,2(0)
         S     15,FIXVIER              I*K-1 IN REG. 15
         L     6,8(1)                  C
         LA    7,0(0)                  IC = 1 IN REG. 7
         SDR   0,0                     CLEAR FL.REG. 0
LOOPIC   STE   0,0(7,6)                C(IC) = 0.
         LA    7,4(7)                  IC = IC + 1
         CR    7,15
         BC    12,LOOPIC
         B     TESTJ
TESTIK   L     10,12(1)
         L     10,0(10)                I IM REG. 10
         L     15,20(1)
         L     15,0(15)                K IN REG. 15
         MR    14,10                   I*K IN REG. 15
         LTR   15,15
         BC    9,RETURN1               IF I=0 OR K=0 RETURN
TESTJ    L     3,16(1)
         L     3,0(3)                  J IN REG. 3
         LTR   3,3
         BC    9,RETURN1
STOSA    L     15,12(1)            I IN REG. 15
          L    15,0(15)
         ST    15,I                SAVE I
         L     15,20(1)            K IN REG. 15
         L     15,0(15)
         ST    15,K                SAVE K
         LTR   4,4
         BC    9,DREI                  TEST ENTRY
         L     10,I
         SLL   10,2(0)                 IIA = 4*I IN REG. 10
         MVC   IOA,FIXVIER             IOA = 4*1
         SLL   3,2(0)                  4*J IN REG. 3
         B     SIEBEN
DREI     LA    10,4(0)                 IIA = 1*4
         L     3,16(1)                 J
         L     3,0(3)
         SLL   3,2(0)                  4*J IN REG. 3
         ST    3,IOA                   IOA = 4*J
SIEBEN   LTR   5,5
         BC    9,NEUN                  TEST ENTRY
         LA    11,4(0)                 IIB = 1*4 IN REG. 11
         ST    3,IOB                   IOB = 4*J
         B     DREIZEHN
NEUN     LR    11,15
         SLL   11,2(0)                 IIB = 4*K IN REG. 11
         MVC   IOB,FIXVIER             IOB = 1*4
DREIZEHN MVC   IA,FIXNULL              IA = 0*1
         LA    7,0(0)                  IC = 0*1 IN REG. 7
         L     4,0(1)                  A
         L     5,4(1)                  B
         L     6,8(1)                  C
         LA    14,1(0)                 L = 1 IN REG. 14
LOOPL    L     15,K
         LA    0,0(0)              IB=1 IN REG. 0
         LA    12,1(0)                 M = 1 IN REG. 12
LOOPM    L     8,IA                    JA = IA IN REG. 8
         LR    9,0                     JB = IB IN REG. 9
         LA    1,4(0)                  N = 1*4 IN REG. 1
         SDR   0,0                     CLEAR FL.REG. 0
         SDR   4,4                     CLEAR FL.REG. 4
LOOPN    LE    2,0(8,4)                A(JA) IN FL.REG. 2
         ME    2,0(9,5)
         ADR   4,2
         AR    8,10                    JA = JA + IIA
         AR    9,11                    JB = JB + IIB
         LA    1,4(1)                  INCREASE N
         CR    1,3
         BC    12,LOOPN
         LE    0,0(7,6)
         ADR   0,4
         STE   0,0(7,6)                C = C + A*B
         A     0,IOB                   IB = IB + IOB
ZWEIDREI LA    7,4(7)                  IC = IC + 1
         LA    12,1(12)                INCREASE M
         CR    12,15
         BC    12,LOOPM
         L     1,IA
         A     1,IOA
         ST    1,IA                    IA = IA + IOA
         LA    14,1(14)                INCREASE L
         L     15,I
         CR    14,15
         BC    12,LOOPL
*
*  E P I L O G U E
*
EPILOGUE EQU   *
RETURN1  EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         MVI   12(R13),X'FF'
         BR    R14
*
*  D A T A   A R E A S
*
I        DS    1F
K        DS    1F
FIXVIER  DC    F'4'
FIXNULL  DC    F'0'
IA       DS    1F
IOA      DS    1F
IOB      DS    1F
*
         END
#ifdef CERNLIB_TCGEN_MXMAD
#undef CERNLIB_TCGEN_MXMAD
#endif
