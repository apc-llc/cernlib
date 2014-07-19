*
* $Id: abuser.s,v 1.1.1.1 1996/02/15 17:51:48 mclareni Exp $
*
* $Log: abuser.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:48  mclareni
* Kernlib
*
*
         PRINT NOGEN
ABUSER   CSECT
#if defined(CERNLIB_QMIBMXA)
ABUSER   AMODE ANY
ABUSER   RMODE ANY
#endif
*
* CERN PROGLIB# Z036    ABUSER          .VERSION KERNIBM  2.25  881103
*
* System :    VM / CMS
*
* Function:  Saves the address of a user specified exit routine
*            to which control is passed after an abend
*
* Called by:  EXTERNAL NAME
*             CALL ABUSER(NAME)  To set up user exit
*             CALL ABUSER(0)     To cancel existing exit
*        or:  CALL REENT(NAME)   to set up user exit
*             CALL REENT(0)      to cancel existing exit
*
*             ABUSER may be called at any time before an abend
*             to specify, respecify or cancel the user exit routine.
*
* Calls:      After an ABEND control is transferred to the user exit
*             routine via a call of the form CALL NAME(KODES,KODEU).
*             The two arguments kept only for compatibility with MVS
*             and are set to zero.
*             On return from the user exit routine control passes on
*             to the standard Fortran ABEND processing routines.
*
* Notes:      ABUSER uses storage in NUCON which must not be
*             overwritten:   8bytes at NUSERFWD
*                            8bytes at NUCCOPYR for CMS4 and previous
*                                      NUCUSER1 for CMS5 on
*
*********************************************************************
*
* Modification log:
*
* 1.00 Original version   Tony Cass  CERN/DD   19th July 1988
*
*********************************************************************
*
* 1.01 For XA change DMSKEY NUCLEUS to SPKA 224(0)
*                    DMSKEY USER       SPKA 0
*      For CMS 5 onwards use NUCUSER1 & NUCUSER2 instead of NUCCOPYR
*
* Tony Cass  CERN/DD   3rd November 1988
*
*********************************************************************
*
* 1.02 VS Fortran 2.5 generates an operation exception testing for
*      presence of hardware SQRT instruction. Add call to S#QRT so that
*      this exception does not lead to call to user exit routine.
*
* Tony Cass  CERN/DD   7th January 1992
*
*********************************************************************
*
         PRINT NOGEN
         ENTRY REENT
REENT    EQU   *
*
         USING ABUSER,R15
         B     SAVE
         DC    AL1(7),CL7'ABUSER' Name
         DC    CL4'1.02'          Version.modification
SAVE     STM   R14,R12,12(R13)
         LR    R4,R13
         CNOP  0,4
         BAL   R13,CARRYON
ABUSSAVE DC    18F'0'
         DROP  R15
         USING ABUSSAVE,R13
CARRYON  ST    R13,8(,R4)
         ST    R4,R4(,R13)
         XC    8(4,R13),8(R13)
*
         USING NUCON,R0
*
         L     R1,0(,R1)          -> address of user exit routine
         L     R1,0(,R1)          =  ditto
         C     R1,=F'0'           Setting or unsetting?
         BE    UNSET
*
SET      ST    R1,USEREXIT        Save it so I know where to go
         TM    PSWSTATE,MINE      Is s/he resetting exit address?
         BNZ   RETURN             Yup, so I've finished
*
* Make sure the operation exception generated when Fortran tests for
* hardware SQRT won't cause a call here.
*
         EXTRN S#QRT
         EXTRN D#SQRT
         LA    R1,SQRTPARM
         L     R15,=A(S#QRT)
         BALR  R14,R15
         LA    R1,SQRTPARM
         L     R15,=A(D#SQRT)
         BALR  R14,R15
*
         LA    R1,HALFWAY         Set up new program interrupt handler
         TM    X'5EA'(0),X'80'    XA mode?
         BNO   SETNPSW            No, basic address is fine
         O     R1,=X'80000000'    Yes, say so ...
*
SETNPSW  SPKA  0(0)
         MVC   PSWSTORE(8),PGMNPSW
         ST    R1,PGMNPSW+4
         SPKA  224(0)
         OI    PSWSTATE,MINE      and show it's been done
*
* Intercept return to system so I can reset PROGNPSW if necessary
*
         USING SVCSECT,R11
         USING SSAVE,R12
*
         TM    THRUME,YES         Check I've not already done this
         BNZ   RETURN
         L     R11,ASVCSECT       Follow the tortuous path
         L     R12,CURRALOC       to get the address of the
         L     R1,USAVEPTR        save area of 'caller'
         MVC   TOSYSTEM(4),12(R1) Save the return address
         LA    R5,PUTBACK
         TM    X'5EA'(0),X'80'    XA mode?
         BNO   SETRETN            No, basic address is fine
         O     R5,=X'80000000'    Yes, say so ...
*
SETRETN  ST    R5,12(,R1)         and put mine there instead
         OI    THRUME,YES         Flag it as done
*
         B     RETURN
*
UNSET    TM    PSWSTATE,MINE      Check psw needs resetting
         BZ    RETURN
         SPKA  0(0)               Yes, put back cms interrupt handler
         MVC   PGMNPSW(8),PSWSTORE
         SPKA  224(0)
         NI    PSWSTATE,255-MINE  and show it's been done
*
RETURN   L     R13,4(,R13)
         LM    R14,R12,12(R13)
         OI    12(R13),X'FF'
         BR    R14
*
* Here when the ABEND happens
*
         DROP  R13
HALFWAY  ST    R13,NUSERFWD
         BALR  R13,0              Where on earth am I?
         USING *,R13
         CNOP  0,4
         BAL   R13,AROUNDSA       Do this for nice FORTRAN linkage
         DROP  R13
         USING SAVEAREA,R13
SAVEAREA DC    18F'0'
AROUNDSA STM   R14,R12,MYSAVE     and save registers for later
*
* Check which type of interrupt I've got
*
         LA    R1,43              Location of interrupt code
         TM    PGMOPSW+1,BCMODE   depends on control mode
         BZ    WHICH
         LA    R1,143
WHICH    CLI   0(R1),X'06'
         BH    TONORMAL           Ignore maths problems
*
* Now go to exit routine after checking idiot isn't already ABENDing
*
         TM    LOOPING,YES
         BNZ   PROBLEMS
         OI    LOOPING,YES
         SPKA  224(0)             Stop user overwriting nucleus
         LA    R1,AKODES          For compatibility with MVS
         L     R15,USEREXIT
         BALR  R14,R15
         SPKA  0(0)
*
* Be nice and put back real program new PSW
*
RESETPSW SPKA  0(0)
         MVC   PGMNPSW(8),PSWSTORE
         SPKA  224(0)
         NI    PSWSTATE,255-MINE  and show it's been done
*
* Now go to real interrupt processor
*
TONORMAL LM    R14,R12,MYSAVE
         L     R13,NUSERFWD
         LPSW  PSWSTORE           Not PGMNPSW since here for maths also
*
* Here if user's ABEND routine ABENDs...
*
PROBLEMS WRTERM '**************************************************'
         WRTERM '** SORRY, YOUR ABEND PROCESSING ROUTINE ABENDED **'
         WRTERM '**************************************************'
         B     RESETPSW
*
* Here on the way back to CMS - replace PGMNPSW
*
         USING *,R14              Return is BR 14 with R15=return code
PUTBACK  STM   R15,R13,MYSAVE     Just in case...
         TM    PSWSTATE,MINE      Check psw needs resetting
         BZ    ALLOVER            No
         SPKA  0(0)               Yes
         MVC   PGMNPSW(8),PSWSTORE
         SPKA  224(0)
         NI    PSWSTATE,255-MINE  and show it's been done
*
ALLOVER  LM    R15,R13,MYSAVE
         L     R14,TOSYSTEM       Return address to CMS
         BR    R14                And now it really is all over
*
         DS    0D
USEREXIT DC    AL4(0)
TOSYSTEM DC    AL4(0)
MYSAVE   DC    18F'0'
AKODES   DC    AL4(KODES)
AKODEU   DC    AL4(KODEU+X'80000000')
KODES    DC    F'0'
KODEU    DC    F'0'
SQRTPARM DC    AL4(FOUR+X'80000000')
FOUR     DC    F'41400000'
         DC    F'0'
BCMODE   EQU   X'08'
LOOPING  DC    X'00'
THRUME   DC    X'00'
YES      EQU   X'01'
PSWSTATE DC    X'00'
MINE     EQU   X'01'
         DS    0D
         LTORG
         DS    0D
         NUCON
#if defined(CERNLIB_QCMS4)
PSWSTORE EQU   NUCCOPYR           For CMS4 and previous
#endif
#if !defined(CERNLIB_QCMS4)
PSWSTORE EQU   NUCUSER1           For CMS5 [ONWARDS...]
#endif
         SVCSECT
         CMSAVE
         REGEQU
         END
