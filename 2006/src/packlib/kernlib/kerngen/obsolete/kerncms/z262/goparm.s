*
* $Id: goparm.s,v 1.1.1.1 1996/02/15 17:51:49 mclareni Exp $
*
* $Log: goparm.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:49  mclareni
* Kernlib
*
*
         PRINT NOGEN
GOPARM   CSECT
#if defined(CERNLIB_QMIBMXA)
GOPARM   AMODE ANY
GOPARM   RMODE ANY
#endif
*
* CERN PROGLIB# Z262    GOPARM          .VERSION KERNIBM  2.23  880425
* ORIG. 01/12/85 RM, 22/9/87 HRR for VS Fortran Release 2
*
* FORTRAN CALLABLE ROUTINE TO OBTAIN THE PARM STRING PASSED WITH A
* START OR MODULE-NAME COMMAND IN VM/CMS RELEASE 3 WITH VS FORTRAN
* RELEASE 2. A TRIVIAL CHANGE IS NEEDED FOR VS FORTRAN RELEASE 1.
*
*              CALL GOPARM(LENGTH,PARMS)
*
* 1ST ARGUMENT= INTEGER LENGTH
* 2ND ARGUMENT= CHARACTER*100 PARMS  OR LOGICAL*1 PARMS(100)
* RETURNED     LENGTH IS THE INTEGER LENGTH OF PARMS
* RETURNED     PARMS IS THE UNTOKENIZED PARM LIST (SEE CMS USERS GUIDE)
*
*  THIS ROUTINE MAY BE USED FOR MODULES VIA
* MODULENAME PARAMETER-STRING
*  OR AFTER A LOAD IN THE START COMMAND VIA
* START ENTRY-NAME PARAMETER-STRING
*  NOTE THAT ANY (UNECESSARY) BRACKETS APPEAR AS PART OF THE STRING.
*  AFTER A LOAD WITH START AS OPTION NO PARAMETERS MAY BE GIVEN AND
*  A LENGTH OF ZERO IS RETURNED. THE MAXIMUM LENGTH RETURNED IS 100
*
*
* NOTE:  THIS VERSION OPERATES IN A VS FORTRAN RELEASE 2 ENVIRONMENT
* ONLY AS IT CALLS AFBVLCIN FROM THE RUN-TIME LIBRARY. FOR VS FORTRAN
* RELEASE 1 CHANGE THIS TO CALL IFYVLCIN.
*
* Version 1.01 January 1988
*
* Modified January 1988 to pick up the address of start and end
* of the parameter string from the Nucleus - this should work for
* all VSFortran Releases.
* Tony Cass CERN/DD
*
* Version 1.02 July 1991
*
* ... but not all CMS releases!
* On entry to the program R0 points to an extended plist, use this.
* Tony Cass CERN/CN
*
*********************************************************************
*
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
R11      EQU   11             ADDRESS OF VS FORTRAN COMMON WORK AREA
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
         PRINT NOGEN
*
*  P R O L O G U E
*
         USING GOPARM,R15
         USING NUCON,0
         B     SAVEREGS
         DC    AL1(7),CL7'GOPARM'    IDENTIFIER
         DC    C'1.02'               VERSION.MODIFICATION LEVEL
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
         LM    R9,R10,0(R1)   R9 -> ARG1, R10 -> ARG2
         XC    0(4,R9),0(R9)  SET A TENTATIVE PARM LENGTH OF ZERO
*
* Find what R0 was when called from SVC save area - version 1.02
*
         USING SVCSECT,R5
         USING SSAVE,R6
         L     R5,ASVCSECT
         L     R6,CURRALOC
         L     R7,EGPR0
         L     R2,4(,R7)
         L     R3,8(,R7)
         L     R7,0(,R7)
*
* use nucleus plist pointers - version 1.01
*
*        L     R2,NUCPLBEG
*        L     R3,NUCPLEND    R2,R3 := ARGSTART, ARGEND
*        L     R7,NUCPLCMD
*
* Common code for versions 1.01 & 1.02
*
         MVC   COMMAND,0(R7)       copy the command,                    GOP01060
         OC    COMMAND,=C'     '    translate to upper case             GOP01070
         CLC   COMMAND,=C'LOAD '    and quit if it was LOAD...          GOP01080
         BE    EPILOGUE                                                 GOP01090
         SR    R3,R2          R3 := PARM LENGTH
         BZ    EPILOGUE       BRANCH IF IT IS ZERO
         C     R3,=F'100'     IS IT GT 100?
         BNH   STLEN          NO, BRANCH
         L     R3,=F'100'     SET IT TO 100
STLEN    ST    R3,0(0,R9)     ELSE STORE IT FOR THE USER
         BCTR  R3,0           SUBTRACT 1 FOR MVC
         EX    R3,MVC         MOVE THE PARM FIELD TO USER'S AREA
*
*  E P I L O G U E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         MVI   12(R13),X'FF'
         BR    R14
*
*  D A T A   A R E A S
*
         DS    0H
MVC      MVC   0(*-*,R10),0(R2)
COMMAND  DS    CL5
*
         LTORG
         NUCON
*
*  Next two equates added for version 1.02
*
         SVCSECT
         CMSAVE
*
         END
