*
* $Id: abuser.s,v 1.1.1.1 1996/02/15 17:53:18 mclareni Exp $
*
* $Log: abuser.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:18  mclareni
* Kernlib
*
*
ABUSER   CSECT
*
* CERN PROGLIB# Z036    ABUSER          .VERSION KERNIBM  2.11  850320
*
*********************************************************************
*
* FUNCTION:     RECORD THE ADDRESS OF A USER EXIT ROUTINE AND
*               SUBSEQUENTLY PASS CONTROL TO IT DURING ABEND
*               PROCESSING.  THE USER EXIT ROUTINE MUST BE ABLE TO
*               RECEIVE A CALL OF THE FORM 'CALL NAME(KODES,KODEU)'
*               WHERE KODES WILL BE THE SYSTEM COMPLETION CODE AND
*               KODEU THE USER COMPLETION CODE.  IT MUST RETURN
*               WHEN ITS PROCESSING IS COMPLETE.
*               ABUSER MAY BE CALLED AT ANY TIME DURING NORMAL
*               PROGRAM EXECUTION TO RE-SPECIFY THE ADDRESS OF THE
*               USER EXIT ROUTINE.  CALL ABUSER(0) CANCELS THE EFFECT
*               OF ALL PREVIOUS CALLS.
*
* CALLING SEQUENCE:     CALL ABUSER(NAME)
*
* ARGUMENTS:     NAME   - THE NAME OF THE USER EXIT ROUTINE WHICH
*                         MUST APPEAR IN A FORTRAN EXTERNAL STATEMENT
*
* ENTRY POINT:     REENT  - CALL REENT(NAME) HAS THE SAME EFFECT AS
*                           CALL ABUSER(NAME)
*
* NOTES:     THIS VERSION IS ONLY FOR USE IN A SIEMENS FORTRAN 77
*            ENVIRONMENT.  IT CALLS THE SIEMENS SERVICE SUBPROGRAM
*            ENDXIT.
*
*********************************************************************
*
* MODIFICATION LOG :
*
* 1.02 : CMS-COMPATIBLE CODE ADDED.
*        R. MATTHEWS, CERN/DD, DECEMBER 1984.
*
* 1.01 : MODIFIED TO TEST THE DETAIL CODE RECEIVED FROM THE FORTRAN
*        LIBRARY;  IF IT IS FOUND TO BE ZERO THE USER EXIT IS NOT
*        CALLED.  THIS MODIFICATION WAS MADE NECESSARY BY A CORRECTION
*        TO V10L20 OF THE FORTRAN LIBRARY WHICH CAUSES THE EXIT
*        IDENTIFIED IN THE CALL TO ENDXIT TO RECEIVE CONTROL FOR NORMAL
*        TERMINATION AS WELL AS ABNORMAL TERMINATION, (PREVIOUSLY
*        CONTROL WAS ONLY RECEIVED FOR ABNORMAL TERMINATION).
*        R. MATTHEWS, CERN/DD, JULY 1984.
*
* 1.00 : ORIGINAL VERSION,  R. MATTHEWS, CERN/DD, DECEMBER 1982.
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
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
         PRINT NOGEN
*
         ENTRY REENT
*
REENT    EQU   *
*
*  P R O L O G U E
*
         USING ABUSER,R15
         B     SAVEREGS
         DC    AL1(7),CL7'ABUSER'    IDENTIFIER
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
*  C A L L   E N D X I T
*
         L     R2,0(0,R1)     R2 -> ADDRESS OF USER EXIT
         L     R2,0(0,R2)     R2 := ADDRESS OF USER EXIT
         LTR   R2,R2          IS IT ZERO ?
         BZ    CANCEL         YES, BRANCH
*
         ST    R2,USEREXIT    STORE IT
         CALL  ENDXIT,(F1,AABEXIT,IRC,F0),VL
         B     EPILOGUE
*
CANCEL   CALL  ENDXIT,(F0,AABEXIT,IRC,F0),VL
         B     EPILOGUE
*
*  C A L L   U S E R   E X I T   W I T H   S C C   A N D   U C C
*
         CNOP  0,4
ABEXIT   EQU   *
         USING ABEXIT,R15     ESTABLISH ADDRESSABILITY
         B     EP0102
EP0101   DC    A(SAVEAREA)
EP0102   STM   R14,R12,12(R13)
         LR    R4,R13
         L     R13,EP0101
         ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
         DROP  R15
*
         LM    R2,R3,0(R1)    R2 -> RETURN CODE;  R3 -> DETAIL CODE
         L     R3,0(0,R3)     R3 := DETAIL CODE
         LTR   R3,R3          IS IT ZERO ?
         BZ    EPILOGUE       YES, DO NOT CALL THE USER EXIT
*
#if defined(CERNLIB_SYCERNV)
         L     R2,0(0,R2)     R3 := RETURN CODE
         ST    R2,UCC         STORE THE RETURN CODE
         ST    R3,SCC         STORE THE DETAIL CODE
#endif
#if !defined(CERNLIB_SYCERNV)
         L     R2,16          R2 -> CVT
         L     R2,0(0,R2)     R2 := CVTTCBP
         L     R2,4(0,R2)     R2 -> TCB
         L     R2,224(0,R2)   R2 -> RTM2WA
         L     R2,28(0,R2)    R2 := COMPLETION CODES
         LA    R2,0(0,R2)     ZERO THE HIGH ORDER BYTE
         SRDL  R2,12          R2 := SYSTEM COMPLETION CODE
         ST    R2,SCC         STORE IT
         SRL   R3,20          R3 := USER COMPLETION CODE
         ST    R3,UCC         STORE IT
#endif
         L     R15,USEREXIT   R15 -> USER EXIT
         CALL  (15),(SCC,UCC),VL
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
AABEXIT  DC    A(ABEXIT)
F0       DC    F'0'
F1       DC    F'1'
IRC      DS    F
SCC      DS    F
UCC      DS    F
USEREXIT DS    F
*
         END
#ifdef CERNLIB_SYOS_REENT
#undef CERNLIB_SYOS_REENT
#endif
#ifdef CERNLIB_SYCERN_ABUSER
#undef CERNLIB_SYCERN_ABUSER
#endif
