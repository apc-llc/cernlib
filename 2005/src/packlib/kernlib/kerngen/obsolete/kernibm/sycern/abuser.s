*
* $Id: abuser.s,v 1.1.1.1 1996/02/15 17:53:19 mclareni Exp $
*
* $Log: abuser.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:19  mclareni
* Kernlib
*
*
ABUSER   CSECT
*
* CERN PROGLIB# Z036    ABUSER          .VERSION KERNIBM  1.11  820328
*
*********************************************************************
*
* CALLING SEQUENCE:     CALL ABUSER(NAME)
*
* FUNCTION:     SUPPLIES THE CERN FORTRAN ABEND PROCESSOR WITH THE
*               NAME OF A USER-SUPPLIED SUBROUTINE TO BE CALLED
*               DURING ABEND PROCESSING.  THE ENTRY POINT ADDRESS
*               OF THE USER-SUPPLIED SUBROUTINE IS STORED IN THE SCB
*               BY ISSUING THE ESTAE MACRO IN OVERRIDE MODE,  (IT IS
*               ASSUMED THAT THE CURRENT SCB IS THE ONE ESTABLISHED
*               BY IHOECOMH).  THE ENTRY POINT ADDRESS IS
*               SUBSEQUENTLY RECEIVED BY IHOSTAE IN THE SDWAPARM
*               FIELD OF THE SDWA.  THE USER-SUPPLIED SUBROUTINE MUST
*               BE CAPABLE OF RECEIVING A CALL OF THE FORM
*               CALL NAME(KODES,KODEU) WHERE KODES WILL BE THE SYSTEM
*               COMPLETION CODE AND KODEU THE USER COMPLETION CODE.
*               ABUSER MAY BE CALLED AT ANY TIME DURING NORMAL PROGRAM
*               EXECUTION, (I.E. BEFORE ABEND), TO RE-SPECIFY THE
*               NAME OF THE USER-SUPPLIED SUBROUTINE.  CALL ABUSER(0)
*               CAUSES THE NAME TO BE CANCELLED.  THE USER-SUPPLIED
*               SUBROUTINE WILL BE CALLED DURING ABEND PROCESSING TO
*               PERFORM PRE-TERMINATION CLEAN UP AND IS INTENDED TO
*               BE A USER-SUPPLIED EXTENSION OF ABEND PROCESSING.
*               THE USER-SUPPLIED SUBROUTINE MUST BE IN VIRTUAL
*               STORAGE WHEN THE ABEND OCCURS (I.E. IT SHOULD BE
*               IN THE ROOT SEGMENT OF AN OVERLAY PROGRAM).
*
* ARGUMENTS:     NAME   - THE NAME OF A USER-SUPPLIED SUBROUTINE.
*                         IT MUST APPEAR IN AN EXTERNAL STATEMENT
*                         IN THE PROGRAM WHICH CALLS ABUSER.
*
*********************************************************************
*
*********************************************************************
*
*                   M O D I F I C A T I O N   L O G
*                   ===============================
*
*********************************************************************
*
*                               1 . 0 1
*                               =======
*
* ENTRY POINT REENT ADDED FOR HYDRA USERS.  CALL REENT(NAME) HAS THE
* SAME EFFECT AS CALL ABUSER(NAME).
*
* R. MATTHEWS, CERN/DD, MAY 1980.
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
*
         LR    R11,R1         R11 -> ARGUMENT LIST
*
         L     R3,0(0,R11)    R3 -> E.P. ADDRESS OF USER ROUTINE
         L     R3,0(0,R3)     R3 := E.P. ADDRESS OF USER ROUTINE
*
         L     R2,16          R2 -> CVT
         L     R2,0(0,R2)     R2 := CVTTCBP
         L     R2,4(0,R2)     R2 -> TCB
         L     R2,160(0,R2)   R2 -> CURRENT SCB
         L     R2,4(0,R2)     R2 := ESTAE EXIT ROUTINE ADDRESS
*
         ESTAE (R2),OV,PARAM=(R3),PURGE=QUIESCE,ASYNCH=NO,TERM=YES
*
*  E P I L O G U E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         OI    12(R13),X'FF'
         BR    R14
*
         END
#ifdef CERNLIB_SYOS_REENT
#undef CERNLIB_SYOS_REENT
#endif
