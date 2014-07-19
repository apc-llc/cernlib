*
* $Id: kaxrclml.s,v 1.1.1.1 1996/03/08 11:40:55 mclareni Exp $
*
* $Log: kaxrclml.s,v $
* Revision 1.1.1.1  1996/03/08 11:40:55  mclareni
* Kapack
*
*
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_QMIBMXA))
         TITLE 'KAXRCL'
***********************************************************************
*                                                                     *
*                             K A X R C L                             *
*                             ===========                             *
*                                                                     *
***********************************************************************
*                                                                     *
* FUNCTION:     OBTAIN THE BLOCKSIZE OF A DIRECT ACCESS FILE FOR USE  *
*               BY KAPACK.  THE BLOCKSIZE IS RETURNED IN THE UNITS    *
*               REQUIRED BY THE FORTRAN OPEN STATEMENT FOR            *
*               UNFORMATTED DIRECT ACCESS FILES, (I.E. BYTES ON IBM). *
*               IN THE EVENT OF AN ERROR A LENGTH OF ZERO WILL BE     *
*               RETURNED.                                             *
*                                                                     *
* CALLING SEQUENCE:     LENGTH = KAXRCL(LUN)                          *
*                                                                     *
* ARGUMENTS:     LUN - AN INTEGER EXPRESSION WHOSE VALUE REPRESENTS   *
*                      THE NUMBER OF THE UNIT TO WHICH THE FILE IS    *
*                      CONNECTED.                                     *
*                                                                     *
***********************************************************************
         EJECT
***********************************************************************
*                                                                     *
*                   M O D I F I C A T I O N   L O G                   *
*                   ===============================                   *
*                                                                     *
***********************************************************************
*                                                                     *
*                               1 . 0 1                               *
*                               =======                               *
*                                                                     *
* DCBOFOPN REPLACED BY X'10' FOR COMPATIBILITY WITH CMS DCBD MACRO.   *
*                                                                     *
* R. MATTHEWS, CERN/DD, APRIL 1985.                                   *
*                                                                     *
***********************************************************************
*                                                                     *
*                               1 . 0 0                               *
*                               =======                               *
*                                                                     *
* ORIGINAL VERSION.                                                   *
*                                                                     *
* R. MATTHEWS, CERN/DD, JULY 1982.                                    *
*                                                                     *
***********************************************************************
         EJECT
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
         EJECT
         PRINT NOGEN
KAXRCL   CSECT
         SPACE 2
*  P R O L O G U E
         SPACE
         USING KAXRCL,R15
         B     SAVEREGS
         DC    AL1(7),CL7'KAXRCL'    IDENTIFIER
         DC    C'1.01'               VERSION.MODIFICATION LEVEL
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
         SPACE
         LR    R11,R1         R11 -> ARGUMENT LIST
         LA    R10,DCB        R10 -> DCB
         USING IHADCB,R10     ESTABLISH ADDRESSABILITY OF THE DCB
         SPACE 2
*  G E N E R A T E   T H E   D D N A M E
         SPACE
         L     R2,0(0,R11)    R2 -> UNIT IDENTIFIER
         L     R2,0(0,R2)     R2 := UNIT IDENTIFIER
         CVD   R2,DOUBLE      CONVERT IT TO EBCDIC...
         MVC   EDITAREA(L'PATTERN1),PATTERN1
         ED    EDITAREA(L'PATTERN1),DOUBLE+6
         MVC   DCBDDNAM+2(2),EDITAREA+1  MOVE IT INTO THE DCB
         SPACE 2
*  O P E N   T H E   D C B
         SPACE
         XC    DOUBLE,DOUBLE  SET DEFAULT LENGTH
         OPEN  DCB
         TM    DCBOFLGS,X'10' WAS THE OPEN SUCCESSFUL ?
         BNO   EPILOGUE       NO, BRANCH
         SPACE 2
*  G E T   T H E   B L O C K S I Z E
         SPACE
         LH    R2,DCBBLKSI    R2 := BLOCKSIZE
         ST    R2,DOUBLE      STORE IT
         SPACE 2
*  C L O S E   T H E   D C B
         SPACE
         CLOSE DCB
         EJECT
*  E P I L O G U E
         SPACE
EPILOGUE EQU   *
         L     R0,DOUBLE      LOAD THE RESULT
         L     R13,4(0,R13)
         L     R14,12(0,R13)
         LM    R1,R12,24(R13)
         MVI   12(R13),X'FF'
         BR    R14
         EJECT
*  D A T A   A R E A S
         SPACE
DOUBLE   DS    D
         SPACE
EDITAREA DS    3C
PATTERN1 DC    X'212020'
         SPACE
DCB      DCB   DDNAME=FT??F001,DSORG=PS,MACRF=(R)
         SPACE 2
         LTORG
         SPACE 2
         DCBD  DSORG=PS
         SPACE 2
         END
#endif
