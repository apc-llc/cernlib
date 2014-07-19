*
* $Id: goparm.s,v 1.1.1.1 1996/02/15 17:53:21 mclareni Exp $
*
* $Log: goparm.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:21  mclareni
* Kernlib
*
*
GOPARM   CSECT
*
* CERN PROGLIB# Z262    GOPARM          .VERSION KERNIBM  1.11  820328
*
*********************************************************************
*
*              C E R N   M O D I F I C A T I O N   L O G
*              =========================================
*
* THIS MODULE HAS BEEN MODIFIED AT CERN.  THE CERN MODIFICATIONS ARE
* DESCRIBED BELOW.
*
*********************************************************************
*
*                             C R N 0 0 1
*                             ===========
*
* THIS MODIFICATION CAUSES GOPARM TO RETURN THE PARAMETER STRING
* WHICH WAS PASSED TO THE CURRENT TASK FROM THE MOTHER TASK.
* THIS ENSURES THAT THE CORRECT PARAMETER STRING IS RETURNED WHEN
* A USER'S PROGRAM IS RUN IN THE FOREGROUND UNDER TSO AND SIMPLIFIES
* THE PROBLEM OF OBTAINING THE CORRECT PARAMETER STRING WHEN THE
* USER'S PROGRAM IS ATTACHED BY THE LOADER.
*
* R. MATTHEWS, NOVEMBER 1979.
*
*********************************************************************
*
*
*
*        SUBROUTINE GOPARM (LENGTH, BYTES)
*
*        RETURNS LENGTH OF GO-STEP PARAMETER STRING IN THE FULLWORD
*        INTEGER VARIABLE "LENGTH", AND THE STRING OF PARAMETER
*        CHARACTERS IN THE STRING "BYTES".  NATURALLY, LENGTH MUST
*        BE CHECKED BEFORE LOOKING AT THE TEXT.
*
*        (NOTE -- IF TREATED AS A FUNCTION, THE VALUE OF GOPARM WILL
*        BE THE LENGTH OF THE PARM STRING.)
*
*        VERSION 0, MOD 0
*        JUNE 18, 1976
*
*        VERSION 0, MOD 1 (USE JOBSTEP TCB, FOR TASKING JOBS)
*        JUNE 29, 1976
*
*
*        WRITTEN BY
*
*              JOHN R. EHRMAN
*              SCS-SCIP, SLAC USER SERVICES GROUP
*              STANFORD LINEAR ACCELERATOR CENTER
*              P.O. BOX 4349
*              STANFORD, CALIFORNIA  94305
*
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
GOPARM   AMODE ANY
GOPARM   RMODE ANY
#endif
         USING *,BASE
         SAVE  (14,5),,GOPARM-V0M1 SAVE CALLER'S REGISTERS
         LM    R1,R2,0(R1)         GET POINTERS TO CALLER'S ARGUMENTS
         SR    R0,R0               INITIALIZE LENGTH TO 0
         ST    R0,0(,R1)           STORE TENTATIVE RESULT LENGTH
         L     R3,16(,0)           GET CVT POINTER
         L     R3,0(,R3)           GET POINTER TO TCB WORDS
         L     R3,4(,R3)           GET POINTER TO CURRENT TCB
         L     R3,TCBFSA(,R3)      GET POINTER TO FIRST PROB PROG SAVE
CHECKIT  DS    0H
         L     R3,24(,R3)          GET VALUE OF R1 AT ENTRY TO MAIN
         LTR   R3,R3               CHECK FOR ZERO, JUST IN CASE
         BZ    NOPARMS             BRANCH IF NO PARMS
         L     R3,0(,R3)           GET POINTER TO PARM DATA
         LTR   R3,R3               CHECK FOR ZERO
         BZ    NOPARMS             BRANCH IF NONE
         LH    R0,0(,R3)           PICK UP PARM STRING LENGTH
         LTR   R0,R0               CHECK FOR NULL PARMS
         BZ    NOPARMS             BRANCH IF ZERO LENGTH
*
*        HAVE A NON-NULL PARM STRING
*
         CH    R0,PARMMAX          CHECK FOR EXCESSIVE LENGTH
         BNH   MOVEPARM            BRANCH IF NOT EXCESSIVE
         LH    R0,PARMMAX          MAX MOVE OF 100
MOVEPARM DS    0H
         ST    R0,0(,R1)           STORE RESULT LENGTH
         LR    R1,R0               MOVE TO AN EXECUTABLE REGISTER
         BCTR  R1,0                DECREMENT BY 1 FOR MOVE
         EX    R1,PARMMVC          MOVE THE PARM STRING
*
NOPARMS  DS    0H
         RETURN  (2,5),T           RETURN TO CALLER
*
PARMMVC  MVC   0(*-*,R2),2(R3)     MOVE PARM STRING TO USER AREA
PARMMAX  DC    Y(PARMLEN)          MAX LEGAL PARM STRING SIZE
*
BASE     EQU   15                  LOCAL BASE REGISTER
PARMLEN  EQU   100                 MAX ALLOWED PARM SIZE
TCBFSA   EQU   112                 OFFSET OF A(FIRST PP SAVEAREA)
TCBJSTCB EQU   124                 OFFSET OF JOBSTEP TCB ADDRESS
TCBLTC   EQU   136                 OFFSET OF LAST SUBTASK OF THIS TCB
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
         END
