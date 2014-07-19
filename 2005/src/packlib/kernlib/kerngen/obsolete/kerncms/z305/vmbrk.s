*
* $Id: vmbrk.s,v 1.1.1.1 1996/02/15 17:51:55 mclareni Exp $
*
* $Log: vmbrk.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:55  mclareni
* Kernlib
*
*
*
* CERN PROGLIB# Z305    VMBRK           .VERSION KERNCMS  1.05  910320
* Author: L.S.Lowe - Birmingham Nov 84
*
#if defined(CERNLIB_QMIBMXA)
VMBRK    RMODE ANY                                                   XA
VMBRK    AMODE ANY                                                   XA
#endif
#if !defined(CERNLIB_QMIBMXA)
         MACRO
         EXTUAREA
EXTUAREA DSECT
EXTUGPRS DS    16F           General registers at interrupt time
EXTUFRS  DS    4D            Floating point registers at interrupt time
EXTUPSW  DS    1D            External Old PSW at interrupt time
         MEND
#endif
VMBRK    CSECT
*
* CALL VMBRK(0)              clears traps
* CALL VMBRK(1,iflagv,itime) sets trap, clears iflagv, set timer if >0
* CALL VMBRK(2)              increment trap inhibit level
* CALL VMBRK(3)              decrement trap inhibit level
* CALL VMBRK(4,igetv)        gets current value of iflagv
* CALL VMBRK(5,iset)         sets new current value of iflagv
*
* This routine allows a program to trap an event such as an immediate
* command BREAK entered, a time expiration, or a software event.
*
* Because a change in program flow is most easily achieved in CMS
* when in an external interrupt exit, because this exit has access
* to the user's registers and PSW, we arrange that an event causes
* an external interrupt to be stacked using CP. When the interrupt
* is taken, the external exit handles the change in program flow.
*
* When changing program flow, we have to ensure that the code we are
* interrupting is user code at the same SVC level as the user program.
*
* The programmer should bear in mind that code optimisation can mean
* that variable values are carried around in registers rather than
* storage. Variables in the routine which sets the trap may not reflect
* their latest value if and when a trap occurs. Note that variables
* that reside in Common always contain their latest values when a call
* to another routine is made.
*
* Author: L.S.Lowe, Birmingham, Nov 84. 021-414-4621.
* Consult author before making external changes to this routine.
*
* Updated LSL 24 Jun 85 to run under MVS and in 370E as well as CMS.
* Updated LSL 29 Aug 89 to remove checks for Fortran I/O
* Updated LSL 29 Aug 89 to run under CMS 5.5 as well as other releases
* Updated LSL 19 Nov 90 changes for 370/XA and 24/31 amode.
* Updated LSL 20 Nov 90 to use specific HNDEXT, and use EXTUAREA dsect.
* Updated LSL 22 Mar 91 to avoid issuing messages during external exit.
* Updated LSL 25 Mar 91 to avoid replacing program mask in BC mode.
*
         USING *,R12
         USING NUCON,0
         SAVE  (14,12),,*               SAVE REGS
         STM   R0,R15,PTEMP-VMBRK(R15)  KEEP REGS
         LR    R12,R15                  GET LOCAL BASE
         LM    R2,R4,0(R1)              PICK UP ARGUMENT ADDRESSES
         L     R2,0(,R2)                LOAD OPTION
         CL    R2,=F'5'                 CHECK OPTION IS IN RANGE
         BH    PBREXIT                  EXIT IF INVALID
         SLL   R2,2                     OPTION TIMES 4
         B     *+4(R2)                  BRANCH ON OPTION
         B     PBRCLR                   0 - CLEAR
         B     PBRSET                   1 - SET
         B     PBRINH                   2 - INHIBIT
         B     PBRUNH                   3 - UNINHIBIT
         B     PBRGET                   4 - GET FLAG
         B     PBRPUT                   5 - PUT FLAG AND CHECK BREAK
*
PBRCLR   TM    PINIT,X'40'              CHECK IF IMMCMD IS SET
         BZ    PBRCLRA                  NO - JUMP
#if defined(CERNLIB_QMIBMXA)
         HNDEXT CLR,CODE=0040           CLEAR EXTERNAL EXIT
#endif
#if !defined(CERNLIB_QMIBMXA)
         HNDEXT CLR                     CLEAR EXTERNAL EXIT
#endif
         IMMCMD CLR,NAME='BREAK'        CLEAR THE IMMEDIATE COMMAND
PBRCLRA  TM    PINIT,X'20'              CHECK IF TIMER WAS SET
         BZ    PBRCLRB                  NO - JUMP
         TTIMER CANCEL                  CLEAR THE TIMER EXIT
PBRCLRB  TM    PINIT,X'01'              TEST BREAK-BYPASSED FLAG
         BZ    PBRCLRX                  NOT SET - JUMP
#if defined(CERNLIB_QMIBMXA)
         APPLMSG TEXT='VMBRK warning: one or more breaks were ignored w*
               hile system code was executing',APPLID=XYZ
#endif
#if !defined(CERNLIB_QMIBMXA)
         LINEDIT TEXT='VMBRK warning: one or more breaks were ignored w*
               hile system code was executing',DOT=NO
#endif
PBRCLRX  MVI   PINIT,X'00'              CLEAR FLAGS
         B     PBREXIT                  AND EXIT
*
PBRSET   MVC   0(4,R3),=F'0'            CLEAR FLAG TO ZERO
         ST    R3,PFLAGAD               KEEP POINTER TO USER'S FLAG
         MVC   PKEEP(64),PTEMP          KEEP REGISTERS ON ENTRY
         ZAP   PINHIBIT,=P'0'           CLEAR INHIBIT LEVEL
         OI    PINIT,X'80'              INDICATE TRAP IS ENABLED
         L     R1,CVTPTR                PICK UP CVT POINTER
         TM    CVTDCB(R1),CVTCMS        CHECK FOR CMS
         BZ    PBREXIT                  NO - BYPASS IMMCMD AND STIMER
         TM    BATFLAGS,X'80'           ARE WE IN ORDINARY IBM CMSBATCH
         BO    PBREXIT                  YES - BYPASS IMMCMD AND STIMER
         L     R5,ASVCSECT              PICK UP SVCSECT ADDR FROM NUCON
         MVC   PDEPTH,DEPTH-SVCSECT(R5) KEEP CURRENT CMS SVC DEPTH
#if defined(CERNLIB_QMIBMXA)
         HNDEXT SET,PEXT,CODE=0040      SET THE EXTERNAL EXIT
#endif
#if !defined(CERNLIB_QMIBMXA)
         HNDEXT SET,PEXT                SET THE EXTERNAL EXIT
#endif
         IMMCMD SET,NAME='BREAK',EXIT=PIMM  SET THE IMMMEDIATE COMMAND
         OI    PINIT,X'40'              INDICATE IMMCMD IS SET
PBRSETT  ICM   R0,15,0(R4)              CHECK FOR A STIMER TIMER VALUE
         BNP   PBREXIT                  NO - JUMP
         STIMER TASK,PTIM,BINTVL=(R4)   SET TIMER EXIT
         OI    PINIT,X'20'              INDICATE TIMER IS SET
         B     PBREXIT                  INITIALISED - EXIT
*
PBRINH   TM    PINIT,X'80'              CHECK THAT TRAP IS ENABLED
         BZ    PBREXIT                  IGNORE IF NOT
         AP    PINHIBIT,=P'1'           INCREMENT INHIBIT LEVEL
         B     PBRCHEX                  EXIT VIA TRAP CHECK
*
PBRUNH   TM    PINIT,X'80'              CHECK THAT TRAP IS ENABLED
         BZ    PBREXIT                  IGNORE IF NOT
         SP    PINHIBIT,=P'1'           DECREMENT INHIBIT LEVEL
         B     PBRCHEX                  EXIT VIA TRAP CHECK
*
PBRGET   TM    PINIT,X'80'              CHECK THAT TRAP IS ENABLED
         BZ    PBREXIT                  IGNORE IF NOT
         L     R6,PFLAGAD               PICK UP USER'S FLAG ADDR
         MVC   0(4,R3),0(R6)            SET USER ARG FROM FLAG VALUE
         B     PBRCHEX                  EXIT VIA TRAP CHECK
*
PBRPUT   TM    PINIT,X'80'              CHECK THAT TRAP IS ENABLED
         BZ    PBREXIT                  IGNORE IF NOT
         L     R6,PFLAGAD               PICK UP USER'S FLAG ADDR
         MVC   0(4,R6),0(R3)            SET FLAG FROM USER ARG
         B     PBRCHEX                  EXIT VIA TRAP CHECK
*
PBRCHEX  CP    PINHIBIT,=P'0'           CHECK INHIBIT LEVEL
         BH    PBREXIT                  NORMAL EXIT IF TRAP INHIBITED
         L     R6,PFLAGAD               PICK UP USER'S FLAG ADDR
         ICM   R0,R15,0(R6)             CHECK VALUE
         BZ    PBREXIT                  NORMAL EXIT IF ZERO
         NI    PINIT,255-X'01'          CLEAR BREAK-BYPASSED FLAG
         LM    R0,R14,PKEEP             LOAD ORIGINAL CALL REGISTERS
         SR    R15,R15                  CLEAR RETURN CODE
         BR    R14                      TRAP TO ORIGINAL CALL POINT
*
PBREXIT  RETURN (14,12),T,RC=0          NORMAL RETURN TO CALLER
         DROP  R12
*
* This closed routine is entered if the timer interval expires.
* An External interrupt is stacked in order to provoke a trap.
*
* Aug 89: this routine no longer uses the code of the external
* interrupt routine, because the stimer exit does not now use
* the register conventions of an external exit (CMS 5.5).
*
PTIM     DS    0H
         USING *,R12
         SAVE  (14,12)                  SAVE REGS
         LR    R12,R15                  GET BASE REG
         L     R3,PFLAGAD               PICK UP FLAG ADDRESS
         MVC   0(4,R3),=F'1'            SET FLAG TO 1 FOR TIMER
         LM    R6,R7,PCPREGS            GET REGS FOR CP COMMAND
         DIAG  R6,R7,X'0008'            STACK AN EXTERNAL INTERRUPT
         RETURN (14,12),T,RC=0          RETURN TO SYSTEM
*
* This closed routine is entered if the user enters a BREAK command.
* An External interrupt is stacked in order to provoke a trap.
*
PIMM     DS    0H
         USING *,R12
         SAVE  (14,12)                  SAVE REGS
         LR    R12,R15                  GET BASE REG
         L     R3,PFLAGAD               PICK UP FLAG ADDRESS
         MVC   0(4,R3),=F'2'            SET FLAG TO 2 FOR IMMCMD
         LM    R6,R7,PCPREGS            GET REGS FOR CP COMMAND
         DIAG  R6,R7,X'0008'            STACK AN EXTERNAL INTERRUPT
         RETURN (14,12),T,RC=0          RETURN TO SYSTEM
*
* This closed routine is entered when an external interrupt is
* received. If the trap is possible, the registers and PSW are
* modified in order to provoke a return to the trapping point.
*
* To cater for various CMS environments, we allow a PSW format of
* BC mode, XA with AMODE 24 (similar to EC mode), or XA with AMODE 31.
* We also allow for the original call of this routine to be via a BALR
* or via a BASR instruction, or indeed from a BR with R14 preset. This
* means that for BC or XA/24 we can't rely on contents of R14 byte 0,
* and so we don't modify PSW byte 4 (ILC/CC/PM for BC, 0 for XA/24).
* Note: an external exit is called in the same AMODE as at HNDEXT SET.
*
PEXT     DS    0H
         USING *,R12
         USING EXTUAREA,R2
         SAVE  (14,12)                  SAVE REGS
         LR    R12,R15                  COPY BASE REG
         LR    R2,R1                    COPY PARM REG
         L     R3,PFLAGAD               PICK UP USER FLAG ADDRESS
         CLC   0(4,R3),=F'0'            SEE IF FLAG IS ALREADY SET
         BNE   PEXTCHI                  JUMP IF YES
         MVC   0(4,R3),=F'2'            SET FLAG TO 2 FOR EXTERNAL
PEXTCHI  DS    0H
         CP    PINHIBIT,=P'0'           CHECK INHIBIT LEVEL
         BH    PEXTRET                  RETURN IF TRAPS ARE INHIBITED
         L     R5,ASVCSECT              PICK UP SVCSECT ADDR FROM NUCON
         CLC   PDEPTH,DEPTH-SVCSECT(R5) CHECK CURRENT SVC DEPTH
         BNE   PEXTSYS                  JUMP IF DIFFERENT SVC LEVEL
         TM    EXTUPSW+0,X'03'          CHECK FOR USER MASK IN PSW
         BNO   PEXTSYS                  JUMP IF NOT NORMAL ENABLES
         TM    EXTUPSW+1,X'E0'          CHECK FOR USER KEY IN PSW
         BNO   PEXTSYS                  JUMP IF NOT IN USER PROTECT
         TM    EXTUPSW+1,X'10'          CHECK FOR USER KEY IN PSW
         BO    PEXTSYS                  JUMP IF NOT IN USER PROTECT
* OK, so set up the registers and PSW to return after original call.
         NI    PINIT,255-X'01'          CLEAR BREAK-BYPASSED FLAG
         MVC   EXTUGPRS(60),PKEEP       COPY KEPT REGISTERS TO EXT AREA
         MVC   EXTUGPRS+60(4),=F'0'     SET USER'S R15 TO ZERO
         LA    R3,1                     AMODE CHECK: MAKE AN
         SLL   R3,24                    ADDR WHICH IS OUTSIDE 24 BITS
         LA    R3,0(,R3)                LOAD THIS ADDR IN USER'S AMODE
         LTR   R3,R3                    AND SEE WHAT HAPPENED
         BNZ   PEXTAM31                 JUMP IF AMODE 31
PEXTAM24 MVC   EXTUPSW+5(3),PKEEP+57    SET 24-BIT ADDR FROM ORIG R14
         B     PEXTRET                  RETURN FROM INTERRUPT
PEXTAM31 MVC   EXTUPSW+4(4),PKEEP+56    SET 31-BIT ADDR FROM ORIG R14
         OI    EXTUPSW+4,X'80'          ENSURE NEW PSW HAS AMODE 31
         B     PEXTRET                  RETURN FROM INTERRUPT
PEXTSYS  OI    PINIT,X'01'              SET BREAK-BYPASSED FLAG
PEXTRET  RETURN (14,12),T,RC=0          RETURN TO SYSTEM
         DROP  R2
*
CVTPTR   EQU   X'10',4
CVTDCB   EQU   X'74',4
CVTCMS   EQU   X'40'
PCPREGS  DC    A(PCPCMND,L'PCPCMND)
PCPCMND  DC    C'EXTERNAL'
PINIT    DC    X'00'
PINHIBIT DC    PL7'0'
PDEPTH   DC    F'0'
PFLAGAD  DC    F'0'
PTEMP    DC    16F'0'
PKEEP    DC    16F'0'
         LTORG
         NUCON
         SVCSECT
         EXTUAREA
         REGEQU
         END
