*
* $Id: vmqfil.s,v 1.1.1.1 1996/02/15 17:51:55 mclareni Exp $
*
* $Log: vmqfil.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:55  mclareni
* Kernlib
*
*
#if defined(CERNLIB_QMIBMXA)
VMQFIL   AMODE ANY
VMQFIL   RMODE ANY
#endif
VMQFIL   CSECT
*   Obtain details on input file. Returns recfm, lrecl, nrecs, nblocks
*   date, time and status.
*   J.D. Shiers CN/AS/PA July 1990
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
         USING VMQFIL,R15
         B     SAVEREGS
         DC    AL1(7),CL7'VMQFIL'    IDENTIFIER
         DC    C'1.00'               VERSION.MODIFICATION LEVEL
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
*        CALL VMQFIL(fn,recfm,lrecl,nrecs,nblocks,chdate,istat,irc)
*
         LM    R2,R9,0(R1)             R2->FN,,,R9->IRC
         LR    R12,R1
         S     R12,=F'4'
         L     R12,0(,R12)
         AR    R12,R1
         L     R12,0(,R12)
         L     R12,0(,R12)             Length of first argument
*
QFILE    EQU   *
         LA    R10,FILENAME            Address of filename
         LA    R11,FILETYPE            Address of filetype
         MVC   0(20,R10),10(R11)       Blank filename
COMPARE  CLI   0(R2),C' '              Look for a blank
         BE    NEXT                    Next field
MOVE     MVC   0(1,R10),0(R2)          Move this character over
         LA    R2,1(R2)                Increment by one character
         LA    R10,1(R10)              Same for output buffer
         S     R12,=F'1'               Countdown length of input string
         LTR   R12,R12                 Have we reached the end?
         BZ    THEBIZZO                Yes, do the bizzo...
         B     COMPARE                 No, go and compare next characte
*
NEXT     EQU   *                       Last character was a blank
         CR    R10,R11                 Trailing blanks?
         BH    THEBIZZO
         LA    R2,1(R2)                Increment pointer
         S     R12,=F'1'               and decrease length counter
         LTR   R12,R12                 Have we reached end of string?
         BZ    THEBIZZO                Yes
         CLI   0(R2),C' '              Look for the next non-blank
         BE    NEXT
         LR    R10,R11                 Address of next field
         LA    R11,FILEMODE            Address of field after that
         B     MOVE                    Go back and move last character
THEBIZZO EQU   *
         LA    R2,FILENAME
*
         FSSTATE (R2),FSCB=TESTFSCB
         LH    R10,26(0,R1)            NRECORDS
         ST    R10,0(R5)
         FSSTATE (R2),FSCB=TESTFSCB,FORM=E
*                            R1->File Status Table (FST)
         MVC   0(1,R3),30(R1)          RECFM (F/V)
         MVC   0(4,R4),32(R1)          LRECL
         MVC   0(4,R6),44(R1)          NBLOCKS
         LA    R10,DATE                Buffer for date
         UNPK  0(13,R10),54(7,R1)      Unpack to 13 byte field
         MVC   0(12,R7),0(R10)         Move back to user's area
         MVC   0(1,R8),31(R1)          ISTAT
         ST    R15,0(,R9)              RETURN CODE
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         L     R14,12(0,R13)
         LM    R2,R12,28(R13)
         MVI   12(R13),X'FF'
         BR    R14
         DS    0D
TESTFSCB FSCB
FILENAME DC    8C' '
FILETYPE DC    8C' '
FILEMODE DC    2C' '
BLANK20  DC    20C' '
DATE     DS    13C
         END
