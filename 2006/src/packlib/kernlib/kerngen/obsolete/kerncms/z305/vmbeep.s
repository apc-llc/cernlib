*
* $Id: vmbeep.s,v 1.1.1.1 1996/02/15 17:51:55 mclareni Exp $
*
* $Log: vmbeep.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:55  mclareni
* Kernlib
*
*
VMBEEP   CSECT
#if defined(CERNLIB_QMIBMXA)
VMBEEP   AMODE ANY
VMBEEP   RMODE ANY
#endif
*
* CERN PROGLIB# Z305    VMBEEP          .VERSION KERNIBM  2.25  881103
* ORIG. 01/01/86  RM
*
*        REFER TO CERN PROGRAM LIBRARY MEMBER Z305 FOR DETAILS
*
*                   M O D I F I C A T I O N   L O G
*                   ===============================
*
*     1 . 0 0
*
*
* ORIGINAL VERSION.
* R. MATTHEWS, CERN/DD, JANUARY 1986.
*
*     1 . 0 1
*
* Add a 'NOP' CCW so VMBEEP works in CMS/370 under CP/XA. This doesn't
* harm HPO behaviour.
*
* Jonathan Wood (?) RAL, October 1988.
*
*     1 . 0 2
*
* Modify to work in 370 or XA mode CMS machines
*
* Tony Cass  CERN/DD  May 1989  -   With thanks to Dick Newson   IBM
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
*  P R O L O G U E
*
         USING VMBEEP,R15
         B     SAVEREGS
         DC    AL1(7),CL7'VMBEEP' IDENTIFIER
         DC    C'1.02'            VERSION.MODIFICATION LEVEL
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
         USING NUCON,0
*
         WAITT                    COMPLETION OF PENDING I/O
         L     R1,=F'-1'          R1 INDICATES CONSOLE DATA REQUIRED
         DC    X'83',X'12',X'0024' DIAGNOSE, CODE X'24'
         BNZ   EPILOGUE           BRANCH IF COMPLETION ABNORMAL
*
#if defined(CERNLIB_QMIBMXA)
         TM    NUCMFLAG,NUCMXA       XA/370?
         BO    BEEPXA                 Yup, better avoid SIOs
*
#endif
         DMSKEY NUCLEUS
         TIO   0(R1)              TEST AVAILABILITY
         BNZ   *-4
         LA    R2,ALARM           R2 := ADDRESS OF ALARM CCW
         ST    R2,CAW             STORE IT IN THE CAW
         SIO   0(R1)              START I/O
         TIO   0(R1)              AWAIT COMPLETION
         BNZ   *-4
         DMSKEY RESET
#if defined(CERNLIB_QMIBMXA)
         B     EPILOGUE
*
BEEPXA   DS    0H
         STNSM RESETIO,X'00'      Disable interrupts
         LR    R2,R1
         GETSID DEVICE=(R2),ERROR=EPILOGUE   Dev code in R1
LOOP1    TSCH  IRB
         BZ    LOOP1
         SSCH  ORB                Start the beeping
LOOP     TSCH  IRB
         BZ    LOOP
         STOSM JUNK,X'00'         Re-enable Interrupts
RESETIO  EQU   *-3
#endif
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
* XA seems to get op-code 0B wrong in the case where it is     XABUG
* the only opcode (ie: one CCW) but not if there are more!     XABUG
* Hence we chain a No-op to the Beep.                          XABUG
*                                                              XABUG
         DS    0D
ALARM    CCW   X'0B',0,X'60',1                                 XABUG
         CCW   X'03',0,X'20',1                                 XABUG
#if defined(CERNLIB_QMIBMXA)
ALARMXA  CCW1  X'0B',0,X'60',1
         CCW1  X'03',0,X'20',1
ORB      DC    F'0'
KEY      DC    B'00000000'
         DC    B'11000000'
         DC    B'11111111'
         DC    B'10000000'
         DC    A(ALARMXA)
IRB      DS    16F
JUNK     DS    X
#endif
*
         LTORG
         NUCON
         END
