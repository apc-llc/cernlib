*
* $Id: vmrexx.s,v 1.1.1.1 1996/02/15 17:51:55 mclareni Exp $
*
* $Log: vmrexx.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:55  mclareni
* Kernlib
*
*
VMREXX   CSECT
#if defined(CERNLIB_QMIBMXA)||defined(CERNLIB_QCMS6)
VMREXX   AMODE ANY
VMREXX   RMODE ANY
#endif
*
* CERN PROGLIB# Z305    VMREXX          .VERSION KERNIBM  2.24  880809
* ORIG. 01/12/85  RM
*
*        REFER TO CERN PROGRAM LIBRARY MEMBER Z305 FOR DETAILS
*
*                   M O D I F I C A T I O N   L O G
*                   ===============================
*
* 3.00  Version for XA capable REXX in CMS 7.
*       The code is essentially that from -QMIBMXA changed to use the
*       APPLMSG macro and SVC 204.
*       Tony Cass, CERN/DD, July 1991.
*
* 2.01  Fix a bug in the code for function 'N' in QMIBMXA and -QMIBMXA
*       versions
*
* 2.00  Rewritten for XA:users a buffer in 24bit addressable memory for
*       interface to REXX; responses are copied to user (flagged v1.04)
*       Tony Cass, CERN/DD, May 1989.
*
* 1.03  Code for functions 'A' and 'B' improved - number in stem.0
*       no longer needs to be left-justified.Code also slightly faster.
*       Tony Cass, CERN/DD, July 1988.
*
* 1.02  Stem variable set and fetch functions added
*       with new function codes 'A' and 'B'
*       Tony Cass, CERN/DD, March 1988.
*
* 1.01  DETECTION OF JZLINIT# USED TO DETERMINE COMPILER ENVIRONMENT TO
*       ENSURE CORRECT OPERATION IN VS FORTRAN LOAD MODE.
*       R. MATTHEWS, CERN/DD, NOVEMBER 1985.
*
* 1.00  ORIGINAL VERSION.
*       R. MATTHEWS, CERN/DD, JULY 1985.
*
*********************************************************************
*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3    -> Return code
R4       EQU   4    -> name portion of acquired storage
R5       EQU   5
R6       EQU   6    -> buffer portion of acquired storage
R7       EQU   7
R8       EQU   8    -> caller's name
R9       EQU   9    Len(caller's name)
R10      EQU   10   -> caller's buffer
R11      EQU   11   LEN(caller's buffer)
R12      EQU   12   -> SHVBLOCK
R13      EQU   13   Base
R14      EQU   14   Holds #bytes acquired by CMSSTOR
R15      EQU   15   Linkage
*
         PRINT NOGEN
*
*  P R O L O G U E
*
         USING VMREXX,R15
         B     SAVEREGS
         DC    AL1(7),CL7'VMREXX'    IDENTIFIER
         DC    C'3.00'               VERSION.MODIFICATION LEVEL
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
#if !defined(CERNLIB_QMIBMXA)||defined(CERNLIB_QCMS6)
         LR    R11,R1         R11 -> ARGUMENT LIST
#endif
#if (defined(CERNLIB_QMIBMXA))&&(!defined(CERNLIB_QCMS6))
         USING SHVBLOCK,R12
#endif
*
*  E N S U R E   T H E   O P E R A T I N G   S Y S T E M   I S   C M S
*
         L     R2,16          R2 -> CVT
         TM    116(R2),X'40'  IS IT CMS ?
         BNO   ERROR1         NO, BRANCH
#if (defined(CERNLIB_QMIBMXA))&&(!defined(CERNLIB_QCMS6))
*
*  P R O C E S S   T H E   A R G U M E N T   L I S T
*
         L     R3,12(,R1)     R3 = Return Code address
         LM    R7,R9,0(R1)
         LR    R10,R9
         LR    R5,R1          R5 -> ARGUMENT LIST
         S     R5,=F'4'       R5 -> ARGUMENT LIST PREFIX
         L     R5,0(0,R5)     R5 := ARGUMENT LIST PREFIX
         L     R0,VJZLINIT    R0 := ADDRESS OF JZLINIT#
         LTR   R0,R0          IS IT PRESENT ?
         BNZ   FUJITSU        YES, BRANCH
*
         AR    R5,R1          R5 -> SECONDARY ARGUMENT LIST
         LM    R5,R6,4(R5)    R5 -> NAME LEN; R6 -> BUFFER/VALUE LEN
         L     R9,0(0,R5)     R9  := NAME LENGTH
         L     R11,0(0,R6)    R10 := BUFFER/VALUE LENGTH
         B     SHVBMAKE
*
FUJITSU  LH    R9,2(0,R5)     R4 := NAME LENGTH
         LH    R11,4(0,R5)     R3 := BUFFER/VALUE LENGTH
#endif
*
*  C O N S T R U C T   T H E   S H V B L O C K
*
#if !defined(CERNLIB_QMIBMXA)||defined(CERNLIB_QCMS6)
         LA    R10,SHVB1      R10 -> SHVBLOCK
         USING SHVBLOCK,R10   MAKE IT ADDRESSABLE USING R10
         MVI   SHVBLOCK,0     ZERO IT...
         MVC   SHVBLOCK+1(SHVBLEN-1),SHVBLOCK
         LM    R2,R4,0(R11)   R2 -> CODE; R3 -> NAME; R4 -> BUFFER/VAL
         MVC   SHVCODE,0(R2)  MOVE THE FUNCTION CODE INTO THE SHVBLOCK
         ST    R3,SHVNAMA     STORE THE NAME POINTER
         ST    R4,SHVVALA     STORE THE BUFFER/VALUE POINTER
         LR    R5,R11         R5 -> ARGUMENT LIST
         S     R5,=F'4'       R5 -> ARGUMENT LIST PREFIX
         L     R5,0(0,R5)     R5 := ARGUMENT LIST PREFIX
         L     R0,VJZLINIT    R0 := ADDRESS OF JZLINIT#
         LTR   R0,R0          IS IT PRESENT ?
         BNZ   FUJITSU        YES, BRANCH
*
         AR    R5,R11         R5 -> SECONDARY ARGUMENT LIST
         LM    R5,R6,4(R5)    R5 -> NAME LEN; R6 -> BUFFER/VALUE LEN
         L     R5,0(0,R5)     R5 := NAME LENGTH
         L     R6,0(0,R6)     R6 := BUFFER/VALUE LENGTH
         B     STORELEN
*
FUJITSU  LH    R6,4(0,R5)     R6 := BUFFER/VALUE LENGTH
         LH    R5,2(0,R5)     R5 := NAME LENGTH
*
STORELEN ST    R5,SHVNAML     STORE THE NAME LENGTH
         ST    R5,SHVUSER     .
         ST    R6,SHVVALL     STORE THE VALUE LENGTH
         ST    R6,SHVBUFL     STORE THE BUFFER LENGTH
*
         CLI   SHVCODE,C'A'   IS THE FUNCTION CODE 'A' ?
         BE    STEM           YES, BRANCH
         CLI   SHVCODE,C'B'   IS THE FUNCTION CODE 'B' ?
         BE    STEM           YES, BRANCH
         CLI   SHVCODE,C'F'   IS THE FUNCTION CODE 'F' ?
         BE    FETCH          YES, BRANCH
*
* The function code 'f' in the next line MUST be in lowercase
*
         CLI   SHVCODE,C'f'   IS THE FUNCTION CODE 'f' ?
         BE    FETCH          YES, BRANCH
         CLI   SHVCODE,C'N'   IS THE FUNCTION CODE 'N' ?
         BNE   DOCMD          NO, BRANCH
         S     R5,=F'2'       ADJUST THE NAME LENGTH FOR USE IN MVC
         MVI   0(R3),C' '     BLANK THE NAME FOR FUNCTION CODE 'N'
         EX    R5,CLRNAME     .
         B     FETCH
*
CLRBUFF  MVC   1(*-*,R4),0(R4)
CLRNAME  MVC   1(*-*,R3),0(R3)
*
FETCH    S     R6,=F'2'       ADJUST THE BUFFER LENGTH FOR USE IN MVC
         MVI   0(R4),C' '     BLANK THE BUFFER FOR FUNCTION CODE 'F'
         EX    R6,CLRBUFF     .
#endif
#if (defined(CERNLIB_QMIBMXA))&&(!defined(CERNLIB_QCMS6))
SHVBMAKE LR    R14,R9
         AR    R14,R11           Add name + value lengths and SHVBLOCK
         LA    R14,SHVBLEN(,R14)   length to find storage needed
         CMSSTOR OBTAIN,BYTES=(R14),LOC=BELOW
         LR    R12,R1         R12 -> SHVBLOCK
         ST    R12,ASHVB      Store address in EPLIST
         XC    SHVBLOCK(SHVBLEN),SHVBLOCK     Zero it...
         MVC   SHVCODE,0(R7)  Move the function code into the SHVBLOCK
         LA    R4,32(R1)      This is the address of the name
         ST    R4,SHVNAMA      store it in the SHVBLOCK
         LA    R6,0(R9,R4)    This is the address of the value buffer
         ST    R6,SHVVALA      store it in the SHVBLOCK
         ST    R9,SHVNAML     Store the name length
         ST    R9,SHVUSER      here as well for code 'N'
         ST    R11,SHVBUFL     Store the buffer length
         ST    R11,SHVVALL      and store it as Value length for set
*
         CLI   SHVCODE,C'A'   Is the function code 'A' ?
         BE    STEM           Yes, branch
         CLI   SHVCODE,C'B'   Is the function code 'B' ?
         BE    STEM           Yes, branch
#endif
*
*  I S S U E   T H E   C O M M A N D
*
#if (defined(CERNLIB_QMIBMXA))&&(!defined(CERNLIB_QCMS6))
         STM   R5,R11,MVCLSAVE   Protect the registers
         LR    R5,R9
         MVCL  R4,R8          Move the name below 16MB
         LR    R7,R11
         MVCL  R6,R10         Move buffer contents below 16MB
         LM    R5,R11,MVCLSAVE   and now restore them
*
#endif
#if !defined(CERNLIB_QMIBMXA)||defined(CERNLIB_QCMS6)
DOCMD    EQU   *
         LA    R0,EPLIST      R0 -> EXTENDED PLIST
         LA    R1,PLIST       R1 -> PLIST
#endif
#if !defined(CERNLIB_QMIBMXA)
         ICM   R1,B'1000',=X'02'  INSERT THE 'SUBCOMMAND CALL' FLAG
         SVC   202            ISSUE THE COMMAND
         DC    AL4(1)
#endif
#if defined(CERNLIB_QCMS6)
         L     R15,=X'0200B000'
         SVC   204            ISSUE THE COMMAND
#endif
#if !defined(CERNLIB_QMIBMXA)||defined(CERNLIB_QCMS6)
         L     R2,12(0,R11)   R2 -> SECOND ARGUMENT
         ST    R15,0(0,R2)    STORE THE RETURN CODE
         LTR   R15,R15        R15 := 0 ?
         BNZ   EPILOGUE       NO, RETURN
         IC    R15,SHVRET     R15 := RETURN CODE FROM THE SHVBLOCK
         ST    R15,0(0,R2)    STORE IT
         B     EPILOGUE
#endif
#if (defined(CERNLIB_QMIBMXA))&&(!defined(CERNLIB_QCMS6))
         LA    R0,EPLIST      R0 -> EXTENDED PLIST
         LA    R1,PLIST       R1 -> PLIST
         L     R15,=X'0200B000'
         SVC   204            ISSUE THE COMMAND
         ST    R15,0(0,R3)    STORE THE RETURN CODE
         LTR   R15,R15        R15 := 0 ?
         BNZ   DROPMEM        NO, RETURN
         IC    R15,SHVRET     R15 := RETURN CODE FROM THE SHVBLOCK
         ST    R15,0(0,R3)    STORE IT
         OI    SHVCODE,X'F0'  Force upper case function
         CLI   SHVCODE,C'S'   Set?
         BE    DROPMEM        Yup, finished
         CLI   SHVCODE,C'D'   Drop?
         BE    DROPMEM        Yup, finished
         L     R7,SHVVALL     Length of value returned
         ICM   R7,B'1000',=C' '  Pad with blanks
         MVCL  R10,R6         Move back to user
         CLI   SHVCODE,C'N'   Fetching variable names too?
         BNE   DROPMEM        No, finished
         L     R5,SHVNAML     Length of name returned
         ICM   R5,B'1000',=C' '  Pad with blanks
         MVCL  R8,R4          Move name back to user
DROPMEM  CMSSTOR RELEASE,BYTES=(R14),ADDR=(R12)
         B     EPILOGUE
#endif
*
*  S T E M   F E T C H / S T O R E   F U N C T I O N S
*
#if !defined(CERNLIB_QMIBMXA)||defined(CERNLIB_QCMS6)
MOVENAME MVC   STEMBUFF(*-*),0(R3)
MOVESUBS MVC   0(*-*,R2),LEOUT+1
*
STEM     S     R5,=F'1'       FOR MVC
         EX    R5,MOVENAME    MOVE NAME INTO MY BUFFER
         LA    R3,STEMBUFF
         ST    R3,SHVNAMA     AND POINT THERE IN SHVBLOCK
         LA    R5,1(R5)       REAL LENGTH
         LA    R2,0(R5,R3)    NEXT FREE CHARACTER IN STEMBUFF
         MVI   0(R2),C'0'     STEM.0 HOLDS NUMBER OF ELEMENTS
         LA    R1,1(R5)       NEW LENGTH WITH SUBSCRIPT
         ST    R1,SHVNAML     STORE IT
         ST    R1,SHVUSER
*
         LA    R3,0           DON'T NEED R3 AS POINTER ANYMORE
         CLI   SHVCODE,C'B'   SO USE IT AS FLAG TO
         BNE   GETZERO        CHOOSE BETWEEN
         LA    R3,1           FETCH AND SET FUNCTIONS
GETZERO  MVI   SHVCODE,C'F'
*
         LA    R9,ZEROBUFF    BUFFER FOR FIRST FETCH
         ST    R9,SHVVALA     STORE
         LA    R1,9           9 DIGITS SO NO PROBLEMS WITH CVB
         ST    R1,SHVBUFL     STORE
*
         LA    R0,EPLIST      R0 -> EXTENDED PLIST
         LA    R1,PLIST       R1 -> PLIST
#endif
#if defined(CERNLIB_QCMS6)
         L     R15,=X'0200B000'
         STM   R15,R1,CALLREGS   Makes life easier...
*
         L     R14,12(0,R11)  REMEMBER ADDRESS OF RETURN CODE
*
         SVC   204            ISSUE THE COMMAND
#endif
#if !defined(CERNLIB_QMIBMXA)
         ICM   R1,B'1000',=X'02'  INSERT THE 'SUBCOMMAND CALL' FLAG
*
         L     R14,12(0,R11)  REMEMBER ADDRESS OF RETURN CODE
         LR    R11,R0         KEEP R0 AND R1 FOR USE BY SVC202
         LR    R12,R1         AS THEY ARE DESTROYED BY LINEDIT
*
         SVC   202            ISSUE THE COMMAND
         DC    AL4(1)
#endif
#if !defined(CERNLIB_QMIBMXA)||defined(CERNLIB_QCMS6)
         LTR   R15,R15        CHECK RETURN CODE
         BZ    SHVCHECK
         ST    R15,0(R14)     PROBLEMS WITH EXECCOMM
         B     EPILOGUE
SHVCHECK ICM   R15,B'0001',SHVRET
         BZ    NUMCHECK
         ST    R15,0(R14)     PROBABLY BAD VARIABLE NAME
         B     EPILOGUE
*
* CHANGE LEFT ADJUSTED STRING FROM FETCH TO RIGHT ADJUSTED
* IN PCVBUFF FOR PACK AND CVB. FETCH SETS LENGTH OF STRING RETURNED
*
NUMCHECK MVC   PCVBUFF(10),BLANKS
         L     R1,SHVVALL           LENGTH OF STEM.0 SET
         S     R1,=F'1'             ONE LESS TO POINT AT LAST CHARACTER
         LA    R7,EPCVBUFF
         SR    R7,R1
*
COMPARE  IC    R8,0(R1,R9)          R9 POINTS TO ZEROBUFF STILL
*
         CLM   R8,B'0001',=C' '     JUNK LEADING BLANKS
         BNE   NOTBLANK
         L     R8,SHVVALL           BUT MAKE SURE THERE
         S     R8,=F'1'             WAS AT LEAST ONE
         CR    R1,R8                VALID DIGIT
         BE    NUMERR
         B     CONVERT
*
NOTBLANK CLM   R8,B'0001',=C'9'     BUT IS
         BH    NUMERR               IT A
         CLM   R8,B'0001',=C'0'     VALID
         BL    NUMERR               DIGIT?
*
         STC   R8,0(R1,R7)          YES, SO MOVE IT
*
NEXT     S     R1,=F'1'             ALTER POINTER FOR NEXT CHARACTER
         BNM   COMPARE              AND ON WE GO
*
CONVERT  PACK  PCVBUFF(10),PCVBUFF(10)    ZONED TO PACKED CONVERSION
         CVB   R8,CVBSTART                NOW A HEX INTEGER
*
* NOW TO READ IN ALL THE VALUES
*
         ST    R4,SHVVALA    PUT BACK REAL BUFFER ADDRESS
         ST    R6,SHVBUFL    AND LENGTH
         ST    R6,SHVVALL    AND LENGTH
         S     R6,=F'2'      ADJUST THE BUFFER LENGTH FOR USE IN MVC
*
         LTR   R3,R3         CHANGE OF SHVCODE
         BZ    REALWORK      NEEDED FOR
         MVI   SHVCODE,C'S'  SET FUNCTION
*
#endif
#if !defined(CERNLIB_QMIBMXA)
REALWORK LA    R7,1
         LA    R9,1
         MVC   LEOUT(10),ZEROS  ONLY FIRST TIME AS NUMBERS GET BIGGER
*
DOTHEM   LINEDIT TEXTA=LEIN,DOT=NO,SUB=(DEC,(R7)),DISP=NONE,BUFFA=LEOUT
         SR    R1,R1
TESTLEN  IC    R1,LEOUT(R9)  FIND OUT HOW LONG THE SUBSCRIPT IS
         LTR   R1,R1
         BZ    GOTLEN        BY LOOKING FOR FIRST '00' BYTE
         LA    R9,1(0,R9)    IN LEOUT BUFFER
         B     TESTLEN
GOTLEN   S     R9,=F'1'      SUBSCRIPT IS ONE SHORTER
         EX    R9,MOVESUBS
         LA    R1,0(R9,R5)   LENGTH FOR EXECCOMM
         ST    R1,SHVNAML    STORE IT
         ST    R1,SHVUSER
*
         LTR   R3,R3          CLEAR BUFFER FOR FETCH
         BNZ   NOCLEAR
         MVI   0(R4),C' '
         EX    R6,CLRBUFF
*
NOCLEAR  LR    R0,R11
         LR    R1,R12
         SVC   202            ISSUE THE COMMAND
         DC    AL4(1)
#endif
#if defined(CERNLIB_QCMS6)
REALWORK LA    R7,1
         LR    R9,R5         Length of name including 0
         S     R5,=F'1'
         LA    R5,STEMBUFF(R5)  R5 now points to '.' of stem
         XC    1(10,R5),1(R5)   Zero stem number position
*
DOTHEM   MVI   0(R5),X'0A'   APPLMSG buffer is 10bytes long
         APPLMSG APPLID=CMS,TEXT='&&1',SUB=(DEC,(R7)),                 -
               BUFFA=(R5),DISP=NONE
         MVI   0(R5),C'.'    Put in the stem '.'
         SR    R1,R1
TESTLEN  IC    R1,STEMBUFF(R9)  FIND OUT HOW LONG THE SUBSCRIPT IS
         LTR   R1,R1
         BZ    GOTLEN        BY LOOKING FOR FIRST '00' BYTE
         LA    R9,1(0,R9)    IN LEOUT BUFFER
         B     TESTLEN
GOTLEN   ST    R9,SHVNAML    STORE IT
*
         LTR   R3,R3          CLEAR BUFFER FOR FETCH
         BNZ   NOCLEAR
         MVI   0(R4),C' '
         EX    R6,CLRBUFF
*
NOCLEAR  LM    R15,R1,CALLREGS
         SVC   204            ISSUE THE COMMAND
#endif
#if !defined(CERNLIB_QMIBMXA)||defined(CERNLIB_QCMS6)
*
         LA    R4,2(R6,R4)   NEXT ELEMENT. 2 AS R6 CHANGED FOR EXED MVC
         ST    R4,SHVVALA
         LA    R7,1(0,R7)
         CR    R7,R8
         BNH   DOTHEM
*
         SR    R15,R15       GOT/SET THEM ALL - FORCE A
         ST    R15,0(R14)    ZERO RETURN CODE
*
         B     EPILOGUE
*
*  E R R O R   P R O C E S S I N G
*
NUMERR   LA    R15,16         RETURN CODE 16 MEANS THAT CONTENTS
         ST    R15,0(R14)     OF STEM.0 AREN'T A VALID NUMBER
         B     EPILOGUE
*
ERROR1   WTO   MF=(E,WTOLIST1)
         ABEND 999,DUMP
*
*  E P I L O G U E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         L     R14,12(0,R13)
         LM    R2,R12,28(R13)
         MVI   12(R13),X'FF'
         BR    R14
*
*  D A T A   A R E A S
*
EPLIST   DC    A(PLIST)       EXTENDED PARAMETER LIST
BEGARGS  DC    A(0)           .
ENDARGS  DC    A(0)           .
         DC    A(SHVB1)       .
*
PLIST    DC    CL8'EXECCOMM'  PARAMETER LIST
*
SHVB1    DS    XL32           STORAGE FOR THE SHVBLOCK
*
         WXTRN JZLINIT#
VJZLINIT DC    A(JZLINIT#)
*
WTOLIST1 WTO   'CRNVM999 VMREXX: THE OPERATING SYSTEM IS NOT CMS',     &
               ROUTCDE=(11),DESC=(6),MF=L
*
#endif
#if (defined(CERNLIB_QMIBMXA))&&(!defined(CERNLIB_QCMS6))
*R0       EQU   0
*R1       EQU   1
*R2       EQU   2
*R3       EQU   3    -> Return code
*R4       EQU   4    -> Stem name buffer
*R5       EQU   5    Length of stem name: points to '.'
*R6       EQU   6    -> buffer portion of acquired storage
*R7       EQU   7
*R8       EQU   8    counter of items to transfer
*R9       EQU   9    Length of stem + subscript in buffer
*R10      EQU   10   -> caller's buffer
*R11      EQU   11   LEN(caller's buffer)
*R12      EQU   12   -> SHVBLOCK
*R13      EQU   13   Base
*R14      EQU   14   Holds #bytes acquired by CMSSTOR
*R15      EQU   15   Linkage
*
STEM     CMSSTOR OBTAIN,BYTES=64,LOC=BELOW    For my name buffer
         LR    R4,R1          R4 -> My name buffer
         ST    R4,SHVNAMA
         LR    R5,R9          Users name length
         MVCL  R4,R8          Move name into my buffer
         MVI   0(R4),C'0'     STEM.0 HOLDS NUMBER OF ELEMENTS
         SR    R4,1
         LA    R5,1(,R4)      Length of name now
         ST    R5,SHVNAML     Store it
         LR    R4,R1          Remember the address of this storage
*
         MVC   AORB(1),SHVCODE Remember which function is wanted
         MVI   SHVCODE,C'F'
         MVC   SHVBUFL,=F'9'  Max 9 digits so no problems with CVB
*
         LA    R0,EPLIST      R0 -> EXTENDED PLIST
         LA    R1,PLIST       R1 -> PLIST
         L     R15,=X'0200B000'
         STM   R15,R1,CALLREGS  Makes life easier
         SVC   204            ISSUE THE COMMAND
         LTR   R15,R15        CHECK RETURN CODE
         BZ    SHVCHECK
         ST    R15,0(R14)     PROBLEMS WITH EXECCOMM
         B     EPILOGUE
SHVCHECK ICM   R15,B'0001',SHVRET
         BZ    NUMCHECK
         ST    R15,0(R14)     PROBABLY BAD VARIABLE NAME
         B     BUFFDROP
*
* CHANGE LEFT ADJUSTED STRING FROM FETCH TO RIGHT ADJUSTED
* IN PCVBUFF FOR PACK AND CVB. FETCH SETS LENGTH OF STRING RETURNED
*
NUMCHECK MVC   PCVBUFF(10),BLANKS
         L     R1,SHVVALL           LENGTH OF STEM.0 SET
         S     R1,=F'1'             ONE LESS TO POINT AT LAST CHARACTER
         LA    R9,EPCVBUFF
         SR    R9,R1
*
COMPARE  IC    R2,0(R1,R6)          R10 points to value buffer
*
         CLM   R2,B'0001',=C' '     JUNK LEADING BLANKS
         BNE   NOTBLANK
         L     R2,SHVVALL           BUT MAKE SURE THERE
         S     R2,=F'1'             WAS AT LEAST ONE
         CR    R1,R2                VALID DIGIT
         BE    NUMERR
         B     CONVERT
*
NOTBLANK CLM   R2,B'0001',=C'9'     BUT IS
         BH    NUMERR               IT A
         CLM   R2,B'0001',=C'0'     VALID
         BL    NUMERR               DIGIT?
*
         STC   R2,0(R1,R9)          YES, SO MOVE IT
*
NEXT     S     R1,=F'1'             ALTER POINTER FOR NEXT CHARACTER
         BNM   COMPARE              AND ON WE GO
*
CONVERT  PACK  PCVBUFF(10),PCVBUFF(10)    ZONED TO PACKED CONVERSION
         CVB   R2,CVBSTART                NOW A HEX INTEGER
*
* NOW TO READ IN  OR SET ALL THE VALUES
*
         ST    R11,SHVBUFL   Put back real buffer length for fetch
         ST    R11,SHVVALL    and for set
*
         CLI   AORB,C'B'     CHANGE OF SHVCODE
         BNE   REALWORK      NEEDED FOR
         MVI   SHVCODE,C'S'  SET FUNCTION
*
REALWORK LA    R8,1
         LR    R9,R5           Length of name including '0'
         S     R5,=F'2'        Length of stem name
         LA    R5,0(R5,R4)     R5 now points to '.' of stem
         XC    0(10,R5),0(R5)   Zero the stem number position
*
DOTHEM   MVI   0(R5),X'0A'    APPLMSG buffer is 10bytes long
         APPLMSG APPLID=CMS,TEXT='&&1',SUB=(DEC,(R8)),                 -
               BUFFA=(R5),DISP=NONE
         MVI   0(R5),C'.'     Put in the stem '.'
         SR    R1,R1
TESTLEN  IC    R1,0(R9,R4)   Find out how long the new name is
         LTR   R1,R1
         BZ    GOTLEN         by looking for the first '00' byte
         LA    R9,1(0,R9)     in the name buffer
         B     TESTLEN
GOTLEN   ST    R9,SHVNAML    Store it
*
         CLI   AORB,C'B'     Is it a SET ?
         BNE   DOCMD         No, fetch value
         LR    R7,R11
         STM   R6,R11,MVCLSAVE  Save the registers
         MVCL  R6,R10           Move the value into the low buffer
         LM    R6,R11,MVCLSAVE  Restore
         LA    R10,0(R11,R10)   And point to next array element
*
DOCMD    LM    R15,R1,CALLREGS
         SVC   204            ISSUE THE COMMAND
*
         CLI   AORB,C'A'      Is it a FETCH?
         BNE   AGAIN          No, see if we trip again
*
         L     R7,SHVVALL     Length of value returned
         ICM   R7,B'1000',=C' '
         STM   R6,R11,MVCLSAVE  Save the registers
         MVCL  R10,R6           Move the value to the user
         LM    R6,R11,MVCLSAVE  Restore
         LA    R10,0(R11,R10)   Point to next array element
         B     AGAIN            And see if we need more
*
AGAIN    LA    R8,1(,R8)
         CR    R8,R2
         BNH   DOTHEM
*
         SR    R15,R15       GOT/SET THEM ALL - FORCE A
         ST    R15,0(,R3)    ZERO RETURN CODE
*
BUFFDROP CMSSTOR RELEASE,BYTES=64,ADDR=(R4)
         CMSSTOR RELEASE,BYTES=(R14),ADDR=(R12)
         B     EPILOGUE
*
*  E R R O R   P R O C E S S I N G
*
NUMERR   MVC   0(4,R3),=F'16'
         B     BUFFDROP
*
ERROR1   WTO   MF=(E,WTOLIST1)
         ABEND 999,DUMP
*
*  E P I L O G U E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         L     R14,12(0,R13)
         LM    R2,R12,28(R13)
         MVI   12(R13),X'FF'
         BR    R14
*
*  D A T A   A R E A S
*
EPLIST   DC    A(PLIST)       EXTENDED PARAMETER LIST
BEGARGS  DC    A(0)           .
ENDARGS  DC    A(0)           .
ASHVB    DC    A(0)           Address of SHVBLOCK
*
PLIST    DC    CL8'EXECCOMM'  PARAMETER LIST
         DC    2F'-1'
*
MVCLSAVE DC    8F'0'
CALLREGS DC    3F'0'
*
         WXTRN JZLINIT#
VJZLINIT DC    A(JZLINIT#)
*
WTOLIST1 WTO   'CRNVM999 VMREXX: THE OPERATING SYSTEM IS NOT CMS',     &
               ROUTCDE=(11),DESC=(6),MF=L
*
#endif
#if defined(CERNLIB_QCMS6)
CALLREGS DS    3F
#endif
#if !defined(CERNLIB_QMIBMXA)||defined(CERNLIB_QCMS6)
STEMBUFF DC    CL250' '
ZEROBUFF DC    CL10' '
#endif
         CNOP  6,8
PCVBUFF  DC    CL2' '
CVBSTART DC    CL7' '
EPCVBUFF DC    CL1' '
BLANKS   DC    CL10' '
#if !defined(CERNLIB_QMIBMXA)
LEIN     DC    X'0A'
         DC    CL10'..........'
LEOUT    DC    XL10'00'
ZEROS    DC    XL10'00'
#endif
#if (defined(CERNLIB_QMIBMXA))&&(!defined(CERNLIB_QCMS6))
AORB     DS    X
#endif
#if defined(CERNLIB_QCMS6)
LEOUTLEN DC    X'0A'
LEOUT    DC    XL10'00'
#endif
*
         LTORG
*
         SHVBLOCK
*
         END
