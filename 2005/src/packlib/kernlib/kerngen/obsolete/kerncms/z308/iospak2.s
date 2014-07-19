*
* $Id: iospak2.s,v 1.1.1.1 1996/02/15 17:51:57 mclareni Exp $
*
* $Log: iospak2.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:57  mclareni
* Kernlib
*
*
*--------------------------------------------------------------------
         PUNCH 'SPB'              TELL LOADER TO START AT NEXT PAGE
*  The '"' character in previous statement is a X'02'.
*  Do not edit the line without CMS SET INPUT " 02 first.
*--------------------------------------------------------------------
         TITLE 'FULL-SCREEN OUTPUT ROUTINES FOR VM/CMS'
*        FLAG  IOSWRT
*--------------------------------------------------------------------
*        Routine to write a field on full-screen terminal
*--------------------------------------------------------------------
*
*    IRC = IOSWRT(LINE,LENGTH,LINENO,NCOL[,IATTR[,IBUF]])
*    IRC = IOSWRT('CLEAR')
*    IRC = IOSWRT()
*    IRC = IOSWRT('WRITE')
*    IRC = IOSWRT(0)
*    IRC = IOSWRT(IBUF)
*
*       IRC  =  Integer return code: 0 = all ok
*                                   -1 = Terminal busy (CP)
*                                   -2 = Invalid Line Number
*                                   -3 = Invalid Column
*                                   -4 = String too long
*                                   -5 = No room for attribute
*                                   -6 = No Length
*                                   -7 = Interrupt Error
*                                   -8 = Diag Error
*                                  -10 = Device End missing
*                                  -11 = I/O still active
*                                  -12 = Too many arguments
*                                 -2xx = HNDINT SET error 'xx'
*                                 -3xx = WAITD      error 'xx'
*                                 -4xx = HNDINT CLR error 'xx'
*
*    LINE = Character array, Hollerith constant or Integer array;
*           'CLEAR', 'CL' or no parameter clears the screen,
*           'WRITE', 'WR' or 0 writes screen from internal buffer,
*           IBUF as the only parm writes screen from IBUF,
*           subsequent writes to screen are superposed.
*  LENGTH = Length of string in bytes (0 for an empty field).
*           If negative the cursor is moved to LINENO, NCOL, and
*           no new field is generated (IATTR ignored).
*  LINENO = Screen Line number (0 = unchanged).
*    NCOL = Screen Column number (0 = unchanged).
*            If both LINENO and NCOL = 0  the string is appended
*           to last string, in the last field defined.
*   IATTR = Attribute Byte, forces the start of a new field;  if
*           missing, the string is placed in the existing field.
*           It is ignored if LENGTH < 0.
*    IBUF = Optional parameter - when missing an immediate write
*           of the field is executed, otherwise it is must be an
*           INTEGER*4  user buffer used to set up a screen.  The
*           field is just assembled into the buffer,  if it is a
*           zero, the internal buffer is used instead. The first
*           two integers of IBUF  must contain the buffer length
*           in bytes (dimension*4-8 or less) and the amount used
*           by previous calls (0 = buffer empty).  This quantity
*           is updated by the routine.
*
* Note: the screen must be cleared to force CP to relinquish the
*       control.  Error -8 signals the user when CP has regained
*       control of the screen  (in case of a message) destroying
*       all the information displayed;  in this case the display
*       must be rebuilt by the user.
*
*        3270 Attribute Byte layout:
*
*           0      1      2      3      4      5      6      7
*        +-------------+------+------+------+------+------+------+
*        |  see table  | Prot | Num  | Int  | L.P. |  --  | MDT  |
*        +-------------+------+------+------+------+------+------+
*
* Table for Bits 0-1:
*
*       Bits 4-7 ->  0    1    2-9    A-F
*       Bits 2-3   +----+-----------+------+
*         |     00 | 40 |     C0    |  40  |     Unprotected Alphanum.
*         V        |    |           |      |
*               01 | 40 |     C0    |  40  |     Unprotected Numeric
*                  |    +----+      |      |
*               10 |   40    |  C0  |  40  |     Protected Alphanum.
*                  +---------+      |      |
*               11 |      C0        |  40  |     Protected Numeric
*                  +----------------+------+
*
* Bits 4-5:     00  01  10  11
*
*               *   *   *   -     Display
*               -   -   *   -     Intensified
*               -   *   *   -     Selector Pen detectable
*               *   *   *   -     Printable
*
* Bit 6:    Reserved
*
* Bit 7:    Modified Data Tag     (may be set by program write)
                             SPACE 2
* Author: Elia PEROTTO - CERN EP Division - tel. 7672303
*                      - CNUCE/CNR - Pisa - tel. 593268
* VERSION 1.0 - 14 Dec 1987
*         1.1 - 11 Apr 1989  CMS 5.5 compatible (macro HNDINT modified)
                             SPACE 2
IOSWRT   CSECT
#if defined(CERNLIB_QMIBMXA)
IOSWRT   AMODE ANY
IOSWRT   RMODE ANY
#endif
&DT      SETC  'IOSWRT  &SYSDATE&SYSTIME'        For debugging
               SAVE  (14,12),,&DT
         LR    R12,R15
               USING IOSWRT,R12
         LR    R10,R1             Save args pointer
GATE     B     INIT               Fetch real console characteristics
*
         LA    R11,INTBUFF        Default to internal buffer
         MVI   STAT,0             Clear status flags
         LTR   R10,R10            Any Parm?
         BZ    CLEAR              None, clear then
*
         LM    R3,R8,0(R10)       Load args addresses
         LTR   R2,R3              R2 = buffer address
         BH    NOONE              Branch if not last parm
*
         ICM   R1,15,0(R2)        Is it a zero?
         BZ    REWRITE            Yes, rewrite buffer
         BH    USWRITE            Write user buffer
*                                 Negative, EBCDIC string
         CLC   0(2,R2),=C'WR'     Is it a keyword?
         BE    REWRITE            Yes, write then
*
         CLC   0(2,R2),=C'CL'     Is it a keyword?
         BE    CLEAR              Yes, clear then
*
ERROR6   LH    R0,=H'-6'          Code for no length specified
         B     ERROR
                             SPACE 1
NOONE    LR    R1,R4              Save length addr
         ICM   R4,15,0(R4)        R4 = buffer length
         BNL   GO                 Positive
         OI    STAT,CURSOR        Negative, put cursor here
*
GO       ST    R4,LENGTH          Save length here
         LTR   R1,R1              Was it last parm?
         BL    GO2                Yes
*
         LR    R1,R5              Save Line Number addr
         ICM   R5,15,0(R5)        Get Line Number
         BNZ   CHLINE
         OI    STAT,SAMELIN       Do not change line
         B     GO4
                             SPACE 1
CHLINE   BCTR  R5,0               Decrement for displacement form
         CL    R5,LASTLINE        Is it outside screen?
         BNL   ERROR2             -2
*
         ST    R5,LINENO
GO4      LTR   R1,R1              Was it last parm?
         BL    GO2                Yes
*
         LR    R1,R6              Save Column addr
         ICM   R6,15,0(R6)        Get Column
         BNZ   CHCOL              If zero...
         OI    STAT,SAMECOL       ...do not change column
         TM    STAT,NOSBA         Line unchanged too?
         BM    GO5
         MVC   COLUMN,OLDCOL      Get last position
         B     GO5
                             SPACE 1
CHCOL    BCTR  R6,0               Decrement for displacement form
         CL    R6,LINELEN         Is it outside line length?
         BNL   ERROR3             -3
*
         ST    R6,COLUMN          Save Column
GO5      LTR   R1,R1              Was it last parm?
         BNL   GO3                No
         OI    STAT,NOFIELD       No new Field
         B     GO2                Skip Attr & Buffer processing
                             SPACE 1
               USING SCRNBUF,R11
GO3      LTR   R7,R7              Was it last parm?
         BNL   STORE              No, store in buffer
         LA    R0,1
         ST    R0,LDONE           Init LDONE with WCC
         OI    STAT,IMMED         Remember it
         B     GO1
                             SPACE 1
STORE    OI    STAT,SAVE          Assemble field into buffer
         LTR   R11,R8             Copy buffer addr in R11
         BNL   ERROR12            Too many arguments -12
*
         ICM   R1,15,BUFLEN       Is a user buffer there?
         BNZ   GO1                Yes
*
         LA    R11,INTBUFF        No, use our internal buffer
GO1      L     R7,0(R7)           Get Attribute
         ST    R7,ATTR            Save Attribute (for debug)
GO2      LM    R5,R7,LINENO            Load Line, Column & Attribute
         TM    STAT,CURSOR+NOFIELD     Attribute required?
         BNZ   NOATTR                  No attribute
*
         TM    STAT,NOSBA         Concatenate?
         BO    NOATTR             Yes, leave space for Attribute
*
         BCTR  R6,0               Back up Column for Attribute
NOATTR   BAL   R14,GETADDR0
         ICM   R1,15,LDONE        Get used bytes
         BNZ   WCC1               WCC place left
         LA    R1,1               Save place for WCC (first call)
WCC1     LA    R5,BUFFER(R1)      Starting byte
         TM    STAT,NOSBA         Concatenate?
*        BO    SKIPSBA            Yes, skip SBA business
         MVI   0(R5),SBA          Store Set Buffer Address
         STCM  R8,3,1(R5)         Store 2 6-bit screen address parts
         TR    1(2,R5),ATAB       Translate to EBCDIC
         LA    R5,3(R5)           Skip over SBA + addr
         LA    R1,3(R1)           Count SBA + addr
SKIPSBA  LTR   R4,R4              Negative length?
         BL    INSCUR
*
         L     R8,BUFLEN          Get last byte addr
         SR    R8,R4              Subtract String Length
         CR    R1,R8              Is R5 too forward?
         BH    ERROR4             Yes, error -4
*
         TM    STAT,NOFIELD       Start Field required?
         BNZ   NOSF               No Start Field
*
         MVI   0(R5),SF           Deposit Start Field
         N     R7,=F'63'          Clear high bits
         IC    R0,ATAB(R7)        Translate Attribute
         STC   R0,1(R5)           Deposit attribute
         LA    R5,2(R5)           Skip over SF,Attr
         LA    R1,2(R1)           Count SF,Attr
NOSF     LTR   R9,R4              Is it a null String?
         BZ    NULLSTR            Yes
*
         BCTR  R9,0               Decrement for Execute
         EX    R9,MOVESTR         Move String
NULLSTR  LA    R1,0(R4,R1)        Add String length
INSCUR2  ST    R1,LDONE
         A     R4,COLUMN          Calculate end of string
         ST    R4,OLDCOL          Remember last position
         TM    STAT,SAVE          Just save?
         BZ    USWRITE2           No, write to screen
         B     RETURN             Yes
                             SPACE 1
INSCUR   MVI   0(R5),IC           Insert Cursor
         LA    R1,1(R1)           Count IC, no need to update R5
         B     INSCUR2            Branch on negative length
                             SPACE 1
*   3270 Orders:
*
SBA      EQU   X'11'         Set Buffer Address
IC       EQU   X'13'         Insert Cursor
SF       EQU   X'1D'         Start Field
RA       EQU   X'3C'         Repeat to Address
                             SPACE 2
MOVESTR  MVC   0(0,R5),0(R2)      Executed MVC
                             EJECT
*--------------------------------------------------------------------
*        Write a Buffer to Screen
*--------------------------------------------------------------------
USWRITE  LR    R11,R2             User buffer address
USWRITE2 LA    R1,2               Keyboard restore
         B     WCCDONE
                             SPACE 1
REWRITE  LA    R11,INTBUFF        Use our internal buffer
         LA    R1,3               Keyboard restore + Reset MDT
WCCDONE  L     R4,LDONE
         IC    R1,ATAB(R1)        Translate WCC
         STC   R1,WCC             Deposit WCC
         LA    R5,DISPCCW         Get ccw address in R5
         LA    R2,BUFFER
         STCM  R2,B'0111',1(R5)        Store buffer address
         STH   R4,6(R5)                Store byte count
         TS    FLAG               Is the path clear?
         BNZ   ERROR11            No, signal error -11 to caller
*
         L     R6,CONSADDR        Get console address
         BAL   R14,HNDINT         Start interrupt handling        *5.5*
*                                 HNDINT issued past here        *ERROR
         LH    R2,=H'-8'               Error code for I/O error
               DIAG R5,R6,X'58'        Write to screen
         BNZ   ERROR8             Something went wrong in the diagnose
         XR    R2,R2              Clear error code
         CLI   FLAG,0             Is Flag clear
         BZ    NOWAIT             Yes, interrupt already gone
         BAL   R14,WAIT           Wait for I/O to finish          *5.5*
*--------------------------- Wait for I/O ---------------------------
NOWAIT   BAL   R14,HNDINT0        Disable Interrupt Handler       *5.5*
         LTR   R0,R2              Any previous error?
         BNZ   ERROR              Yes, signal error to caller
*
         CLI   FLAG,0             Is Flag clear
         BNZ   ERROR7             No, signal error -7 to caller
*
         TM    CSWST,DE           Attention signalled?
         BZ    ERROR10            No, signal error -10 to caller
*
         CLI   CSWST,X'8E'        Screen in use (busy)?
         BZ    ERROR1             Yes, signal error -1 to caller
*
         MVI   CCWCT,0            Clear Erase/Write flag
*        SH    R4,CSW+6           Subtract residual count
         TM    STAT,IMMED         Should this field be saved?
         BZ    RETURN             Yes, leave pointer
*
         LA    R0,1               Initialize buffer count...
         ST    R0,LDONE           ...to 1 (WCC)
RETURN   EQU   *
ERROR    ST    R0,20(R13)         Save it on caller's R0
               RETURN (14,12)     Return to fortran caller
                             SPACE 1
ERROR1   LH    R0,=H'-1'          Code for Screen Busy for CP
         B     ERROR
                             SPACE 1
ERROR2   LH    R0,=H'-2'          Code for invalid line no.
         B     ERROR
                             SPACE 1
ERROR3   LH    R0,=H'-3'          Code for invalid column
         B     ERROR
                             SPACE 1
ERROR4   LH    R0,=H'-4'          Code for overflow error
         B     ERROR
                             SPACE 1
ERROR5   LH    R0,=H'-5'          Code for underflow error
         B     ERROR
                             SPACE 1
ERROR7   LH    R0,=H'-7'          Error code for Interrupt error
         B     ERROR
                             SPACE 1
ERROR8   MVI   FLAG,0             -8   I/O not started
         B     NOWAIT             Release Interrupt Handling
                             SPACE 1
ERROR10  LH    R0,=H'-10'         Error code for DE miss
         B     ERROR
                             SPACE 1
ERROR11  LH    R0,=H'-11'         Error code for Screen Busy for us
         B     ERROR
                             SPACE 1
ERROR12  LH    R0,=H'-12'         Error code for too many arguments
         B     ERROR
                             SPACE 1
ERROR20  LH    R0,=H'-200'        Error code for HNDINT
         B     ERRADD
                             SPACE 1
ERROR40  LH    R0,=H'-400'        Error code for HNDINT
ERRADD   SR    R0,R15
         B     ERROR
                             SPACE 1
                             EJECT
*        FLAG  IOSDIM
*--------------------------------------------------------------------
*        Routine to retrieve the screen dimensions
*--------------------------------------------------------------------
*
*    CALL IOSDIM(LINES,NCOL)
*
               ENTRY IOSDIM
               USING IOSDIM,R15
IOSDIM         SAVE (14,12)
         L     R12,=A(IOSWRT)     Get Module addressability
               DROP R15
         LM    R7,R8,0(R1)        Get argument addresses
         BAL   R14,GETMODEL       Get 3270 information
         ST    R1,0(R7)           Store number of lines
         ST    R2,0(R8)           Store number of columns
               RETURN (14,12)
                             SPACE 3
*--------------------------------------------------------------------
*        Clear Screen and Internal Buffer
*--------------------------------------------------------------------
CLEAR    OI    CCWCT,X'80'        Set Erase/Write flag
         LA    R11,INTBUFF        Empty internal buffer
         XR    R1,R1              Clear number of...
         ST    R1,LDONE           ...bytes assembled
         B     RETURN
                             EJECT
*--------------------------------------------------------------------
*        Get 3270 buffer address
*--------------------------------------------------------------------
*
GETADDR0 LR    R8,R5              Get line no.
         MH    R8,LINELEN+2       Multiply by line length
         AR    R8,R6              Add column no.
         BL    ERROR5             -5
*
         SRDL  R8,6               Push lower 6 bits into R9
         SRL   R9,2               Add 2 zero bits
         SLDL  R8,8               Pull 8 bits in R8
         BR    R14
                             SPACE 3
*--------------------------------------------------------------------
*        Initialize Device Characteristics
*--------------------------------------------------------------------
INIT     BAL   R14,GETMODEL       Find 3270 model
         B     GATE
                             SPACE 1
GETMODEL XR    R0,R0              Clear error code
         LR    R4,R0              Console address...
         BCTR  R4,0               ...unknown
         DIAG  R4,R5,X'24'        Get console information
         BNZR  R14                Terminal disconnected...
*
         MVI   GATE+1,0           Nop the branch
         STM   R4,R6,CONSADDR     Save it here...
*        STH   R4,HNDINT+24       ...and in HNDINT Plist (CMS 4.0)
*        STH   R4,HNDINT+28       ...and in HNDINT Plist (CMS 5.5)
         XR    R1,R1
         IC    R1,CONSMODL        Get 327X Model number
         BCTR  R1,0               Table Displacement
         IC    R1,MODELTAB(R1)    Get screen lines
         XR    R2,R2
         IC    R2,CONSLLEN        Get line length
         LA    R4,DEVINFO         Buffer address
         L     R5,CONSADDR        Console address
         LA    R6,8               Buffer length
         DIAG  R4,R6,X'8C'        Get model 5 screen dimensions
         LTR   R0,R5              Any error?
         BNZ   MODELX             Yes
*
         LH    R2,DEVINFO+2       Get screen width
         LH    R1,DEVINFO+4       Get screen height
MODELX   STM   R1,R2,LASTLINE        Last screen line
         BR    R14                Back to normal operation
                             SPACE 1
MODELTAB DC    AL1(12,24,35,43,27)     Model 1..5 screen lines
                             EJECT
*--------------------------------------------------------------------
*        Initialize Interrupt Handler
*--------------------------------------------------------------------
*
HNDINT         HNDINT SET,(CON1,NETINT,009,ASAP),ERROR=ERROR20
         BR    R14
                             SPACE 1
*--------------------------------------------------------------------
*        Wait for Console I/O completion
*--------------------------------------------------------------------
*
*                            Wait for console I/O (WAITT does not work)
WAIT           WAITD CON1,ERROR=ERROR30
         BR    R14
                             SPACE 1
ERROR30  LH    R2,=H'-300'        Base error code for WAITD
         SR    R2,R15             Add positive WAITD error
         BR    R14                We must deactivate HNDINT
                             SPACE 1
*--------------------------------------------------------------------
*        Disable Interrupt Handler
*--------------------------------------------------------------------
*
HNDINT0        HNDINT CLR,CON1,ERROR=ERROR40    Stop interrupt handling
         BR    R14
                             EJECT
*--------------------------------------------------------------------
*        DEVICE I/O INTERRUPT HANDLER
*--------------------------------------------------------------------
*              FLAG NETINT
               USING NETINT,R15
NETINT   C     R4,CONSADDR        Is it console?
         BNE   INTPASS            No
*
         CLI   FLAG,X'FF'         Is it our start I/O?
         BNE   INTPASS            No
*
         STM   R2,R3,CSW          Save csw
         TM    CSWST,DE+ATTN      Last interrupt (Device End)?
         BZ    INTEXIT            No, wait for next
*
         MVI   FLAG,0             Yes, clear flag to signal completion
INTPASS  XR    R15,R15            Dispatch main task
INTEXIT  BR    R14                Return to cms interrupt handler
                             SPACE 2
FLAG     DC    X'00'              Synchronization flag (0 = free)
STAT     DC    X'00'              Status flags
SAVE     EQU   X'80'              Save field in buffer
IMMED    EQU   X'40'              Immediate write
SAMELIN  EQU   X'20'              Same Line as previous Field
SAMECOL  EQU   X'10'              Same Col. as previous Field
NOSBA    EQU   SAMELIN+SAMECOL    Concatenate strings
CURSOR   EQU   X'02'              Insert Cursor
NOFIELD  EQU   X'01'              No new field
*
DISPCCW  CCW   X'29',0,SILI,0     Write CCW
CCWCT    EQU   DISPCCW+5          CCW Control Byte
SILI     EQU   X'20'              Suppress incorrect length indication
CSW      DS    D             Saved from last interrupt
CSWST    EQU   CSW+4         CSW Status Byte
ATTN     EQU   X'80'         Attention Bit
DE       EQU   X'04'         Device End Bit
*
DEVINFO  DS    D             Buffer for diagnose 24
CONSADDR DS    2F            Buffer for diagnose 8c
CONSRCLS DS    X             RDEVTYPC Class
CONSRTYP DS    X             RDEVTYPE Type
CONSMODL DS    X             RDEVMDL  Model
CONSLLEN DS    X             RDEVLLEN Line Length
LASTLINE DC    F'24'         Last Screen Line
LINELEN  DC    F'80'         Default line length
LENGTH   DC    F'0'          Arg string length
LINENO   DC    F'1'          Line number
COLUMN   DC    F'2'          First usable column in line 1
ATTR     DC    F'0'          3270 attribute byte
OLDCOL   DC    F'0'
ATAB     DC    C' ABCDEFGHI',X'4A4B4C4D4E4F'     '\.<(+|'
         DC    C'&&JKLMNOPQR',X'5A5B5C5D5E5F'    '!$*);~'
         DC    C'-/STUVWXYZ',X'6A6B6C6D6E6F'     '^,%_>?'
         DC    C'0123456789',X'7A7B7C7D7E7F'     ':#@''="'
                             SPACE 2
         LTORG
*
BLEN     EQU   4096-*+IOSWRT-12
INTBUFF  DC    A(BLEN,0)          Internal buffer length
         DS    (BLEN)X            Internal buffer
ENDMOD   EQU   *
                             SPACE 1
SCRNBUF  DSECT
BUFLEN   DC    A(BLEN)            Length of SCREEN Buffer
LDONE    DC    A(1)               Bytes in SCREEN Buffer
BUFFER   EQU   *
WCC      DS    X
                             SPACE 3
               REGEQU
         END
         TITLE 'MACROS'
*--------------------------------------------------------------------
               MACRO
         PAGE
         PUNCH 'SPB'              TELL LOADER TO START AT NEXT PAGE
               MEND
*--------------------------------------------------------------------
               PAGE               LOAD ON A FULL PAGE BOUNDARY
         TITLE 'FULL-SCREEN INPUT ROUTINES FOR VM/CMS'
*--------------------------------------------------------------------
*        Routine to read a field on full-screen terminal
*--------------------------------------------------------------------
*
*    IRC = IOSRD (ECB)
*    IRC = IOSRD (LINE,LENGTH,LINENO,NCOL)
*    IRC = IOSFLD(LINE,LENGTH,LINENO,NCOL)
*
*       ECB  =  Event Control Block (Integer 0)
*
*       IRC  =  INTEGER RETURN CODE: 0 = All OK (Enter pressed)
*                             R0 1..24 = PFKey pressed
*                             R0    -1 = Terminal Busy
*                             R0    -2 = PA2 Key
*                             R0    -3 = CLEAR Key
*                             R0    -4 = Unsolicited Read (no AID)
*                             R0    -5 = Incorrect Parm Number
*                             R0    -6 = Attention Error
*                             R0    -7 = Attention Miss
*                             R0    -8 = Diagnose Error
*                             R0    -9 = Device End Miss
*      LINE  =  Character array or Hollerith constant
*    LENGTH  =  Length of string in bytes (may be 0)
*    LINENO  =  Screen Line number
*      NCOL  =  Screen Column number
*
* Author: Elia PEROTTO - CERN EP Division - tel. 7672303
*                      - CNUCE/CNR - Pisa - tel. 593268
* VERSION 1.0 - 10 Dec 1987
*
IOSRD    CSECT
#if defined(CERNLIB_QMIBMXA)
IOSRD    AMODE ANY
IOSRD    RMODE ANY
#endif
&DT      SETC  'IOSRD   &SYSDATE&SYSTIME'        For debugging
               SAVE  (14,12),,&DT
         LR    R12,R15
               USING IOSRD,R12
         LR    R10,R1             Save args pointer
GATE     B     INIT               Fetch real console characteristics
*
         LTR   R10,R10            No args?
         BZ    ERROR5             Yes, error -5
*
         LM    R3,R6,0(R10)       Load args addresses
         STM   R3,R6,PARMS        Save parms
         LTR   R1,R3              Is it last and only Parm?
         BL    ECBSET             Yes, just set ECB
*
         LTR   R6,R6              Is it last Parm?
         BNL   ERROR5
*
         XR    R1,R1              No need of ECB any more
         BAL   R14,SETINT         Go set interrupt trap
*
               WAITD CON1    Wait for console ATTN (WAITT doesn't work)
*----------------------------------------------------------------------
         XC    ECBADR,ECBADR      Forget ECB
         TS    FLAG               Is the path clear?
         BNZ   ERRORI6
*
         TM    CSWST,ATTN         Attention signalled?
         BZ    ERRORI7
*
         CLI   CSWST,ATTN         Attention signalled?
         BNE   ERROR256
*
         L     R7,0(R4)           Get length
         CH    R7,=H'4'           More than 4?
         BH    USRBUF             Yes, use his buffer
*
         LA    R3,BUFFER               Use internal buffer
         LA    R7,L'BUFFER-4           Its length (less AID code)
USRBUF   LA    R1,DISPCCW              Get ccw address in R1
         MVI   0(R3),0                 Clear old AID byte
         ST    R3,BUFADR               Save for IOSFLD
         STCM  R3,B'0111',1(R1)        Store buffer address
         OI    5(R1),X'80'             Set Read Modified flag
         STH   R7,6(R1)                Store byte count
         LA    R2,9                    Get console address
               DIAG R1,R2,X'58'        Read screen
         BNZ   ERRORI8            Something went wrong in the diagnose
*
               WAITD CON1    Wait for console I/O (WAITT does not work)
*--------------------------------------------------------------------
         BAL   R14,CLRINT         Go clear interrupt trap
         SH    R7,CSW+6           Subtract residual count
         ST    R7,0(R4)           Update length
         ST    R7,LENGTH          Update length
         A     R7,BUFADR          Find last addr
         ST    R7,EOB             Store as End Of Block
         TM    CSWST,CE+DE        Device End arrived?
         BNO   IOERR9             No, signal error to caller
*
         LA    R7,31              Error code for BUSY
         CLI   CSWST,X'8E'        Device Busy?
         BE    IOERR2             Yes, signal error to caller
*
         LA    R7,3               AID + ADDR1 + ADDR2
         ST    R7,LDONE           Init length done
         IC    R7,0(R3)           Get AID byte
         N     R7,=F'31'          Clear high bits
IOERR2   SLL   R7,1               Multiply by 2
         LH    R1,AIDCODE(R7)     Get PFKey number
         STH   R1,ERC             Save for user
         SLL   R7,1               Multiply by 2
         L     R1,AIDTAB(R7)      Get descriptive word
*
         LR    R7,R3              Load 3270 buffer address
         BAL   R14,GETADDR        Translate to Line/Column
         L     R3,PARMS           Restore user buffer
RETURN   LH    R0,ERC             Signal no errors
ERROR    ST    R0,20(R13)         Save it on caller's R0
         XC    ERC,ERC            Clear return code
               RETURN (14,12)     Return to fortran caller
                                  SPACE 1
IOERR    MVC   0(2,R3),CSWST      Copy CSW Status to User Buffer
         B     ERROR
                                  SPACE 1
ECBSET   BAL   R14,SETINT         Go set interrupt trap
         B     RETURN
                                  SPACE 1
ERROR1   LH    R0,=H'-1'          Error code for Terminal Busy
         B     ERROR
                                  SPACE 1
ERROR5   LH    R0,=H'-5'          No Parms error code
         B     ERROR
                                  SPACE 1
ERRORI6  LH    R0,=H'-6'          Error code for Attn
         B     ERRORI
                                  SPACE 1
ERRORI7  LH    R0,=H'-7'          Error code for Attn miss
         B     ERRORI
                                  SPACE 1
ERROR256 LH    R0,CSWST           Error code for Attn plus
         B     ERRORI
                                  SPACE 1
ERRORI8  LH    R0,=H'-8'          Error code for I/O error
ERRORI   STH   R0,ERC
         BAL   R14,CLRINT         Go clear interrupt trap
         B     RETURN
                                  SPACE 1
IOERR9   LH    R0,=H'-9'          Error code for DE miss
         B     IOERR
                                  SPACE 1
ERC      DC    H'0'               Return code
*
*--------------------------------------------------------------------
*        Initialize Device Characteristics
*--------------------------------------------------------------------
INIT     BAL   R14,GETMODEL
         MVI   GATE+1,0           Nop the branch
         B     GATE
*
GETMODEL XR    R0,R0              Clear error code
         LA    R4,9               Console address
         DIAG  R4,R5,X'24'        Get console information
         STM   R4,R6,CONSINFO     Save it here
         LA    R1,24              Model 2 has 24 lines
         CLI   CONSINFO+10,2      Is it a model 2?
         BE    MODELX             Yes it is
*
         LA    R1,27              Default model 5 lines (27)
         LA    R4,DEVINFO         Buffer address
         LA    R5,9               Console address
         LA    R6,8               Buffer length
         DIAG  R4,R6,X'8C'        Get model 5 screen dimensions
         LTR   R0,R5              Any error?
         BNZ   MODELX             Yes
*
         LH    R1,DEVINFO+2       Get screen width
         ST    R1,LINELEN
         LH    R1,DEVINFO+4       Get screen height
MODELX   LR    R5,R1
         BCTR  R5,0               Put it in displacement form
         ST    R5,LASTLINE        Last screen line
         BR    R14                Back to normal operation
*
*--------------------------------------------------------------------
*        Set I/O Interrupt Trap
*--------------------------------------------------------------------
SETINT   XR    R0,R0
         C     R0,ECBADR          Interr. already cared for?
         BNZR  R14                Yes, HNDINT already set
*
         ST    R1,ECBADR          Save user ECB address or 0
         TS    FLAG               Is the path clear?
         BNZ   ERROR1             No, signal error to caller
*
               HNDINT SET,(CON1,NETINT,009,ASAP) start interrupt handl.
         BR    R14
*
*--------------------------------------------------------------------
*        Clear I/O Interrupt Trap
*--------------------------------------------------------------------
CLRINT         HNDINT CLR,CON1    Stop interrupt handling
         BR    R14
*
*--------------------------------------------------------------------
*        DEVICE I/O INTERRUPT HANDLER
*--------------------------------------------------------------------
NETINT   EQU   *
               USING *,R15
         CH    R4,=H'9'           Is it console?
         BNE   INTPASS            No
*
         CLI   FLAG,X'FF'         Is it our start i/o?
         BNE   INTPASS            No
*
         STM   R2,R3,CSW          Save CSW
         TM    CSWST,DE+ATTN      Last interrupt (Device End)?
         BZ    INTEXIT            No, wait for next
*
         MVI   FLAG,0             Yes, clear flag to signal completion
         ICM   R1,15,ECBADR       Get ECB address if any
         BZ    INTPASS            None
*
         ST    R3,0(R1)           Store CSW Status and residual count
INTPASS  XR    R15,R15            Dispatch main task
         BR    R14                Return to cms interrupt handler
*
INTEXIT  STM   R2,R3,CSW1         Save CSW
         BR    R14                Return to cms interrupt handler
*
FLAG     DC    X'00'              Synchronization flag (0 = free)
*
DISPCCW  CCW   X'2A',0,SILI,0     Read CCW
SILI     EQU   X'20'              Suppress incorrect length indication
CSW1     DS    D                  Saved from first interrupt
CSW      DS    D                  Saved from last interrupt
CSWST    EQU   CSW+4         CSW Status Byte
ATTN     EQU   X'80'         Attention Bit
CE       EQU   X'08'         Channel End Bit
DE       EQU   X'04'         Device  End Bit
*
DEVINFO  DS    0D            Buffer for diagnose 24
CONSINFO DS    3F            Buffer for diagnose 8c
LASTLINE DC    F'23'         2nd input line
LINELEN  DC    F'80'         Default line length
PARMS    DS    4F
LENGTH   DC    F'0'          Bytes read
LDONE    DC    F'0'          Bytes seen
BUFADR   DS    A             User or Our buffer address
ECBADR   DC    A(0)          Event Control Block address
                             EJECT
*--------------------------------------------------------------------
*        FIELD ENTRY POINT
*--------------------------------------------------------------------
*
               ENTRY IOSFLD
               USING IOSFLD,R15
&DT      SETC  'IOSFLD  &SYSDATE&SYSTIME'        For debugging
IOSFLD         SAVE  (14,12),,&DT
IOSF2    L     R12,=A(IOSRD)
               DROP R15
         LM    R3,R6,0(R1)        Load args addresses
*                                 R3 = buffer address
*                                 R4 = buffer length addr
*                                 R5 = line number addr
*                                 R6 = column number addr
         XR    R2,R2
         BCTR  R2,0               Set to -1
         L     R7,LDONE           Load Pointer
         C     R7,LENGTH          Check if finished
         BNL   EOS2               Finished already
*
         STM   R3,R6,PARMS        Save parms
         A     R7,BUFADR
         BAL   R14,GETADDR
         L     R8,0(R4)           Get Length
         XR    R2,R2              Init length
LOOP     C     R7,EOB             Is it End Of Block?
         BNL   EOS                Yes
*
         CLI   0(R7),X'11'        Is it SBA?
         BE    EOF                Yes, End Of Field
*
         LA    R2,1(R2)           Count next byte
         CR    R2,R8              Past user buffer?
         BH    USRFULL            Yes
*
         MVC   0(1,R3),0(R7)      Move 1 byte
         LA    R3,1(R3)           Update Target Pointer
USRFULL  LA    R7,1(R7)           Update Source Pointer
         B     LOOP               No
                             SPACE 1
EOF      S     R7,BUFADR          Get length
EOS      ST    R7,LDONE           Store length done
EOS2     ST    R2,0(R4)           Store String (real) Length
               RETURN (14,12)     Return to fortran caller
                             EJECT
*--------------------------------------------------------------------
*        Translate Buffer Address to Line/Column
*--------------------------------------------------------------------
*
GETADDR  LA    R7,1(R7)           Skip AID or SBA
         XR    R8,R8
         ICM   R8,3,0(R7)         Load in addr
         LA    R7,2(R7)           Skip Addr
         SRDL  R8,6               Push lower 6 bits
         SRL   R8,2               Discard 2 bits
         SRDL  R8,6               Push upper 6 bits
         SRL   R9,20              Right adjust the 12 bits
         XR    R8,R8              Clear for divide
         D     R8,LINELEN         Divide by line length
         LA    R8,1(R8)           Start from 1
         LA    R9,1(R9)             "    "   "
         ST    R8,0(R6)           Store column
         ST    R9,0(R5)           Store line
         BR    R14
                             SPACE 2
LINE     DS    F
COLUMN   DS    F
EOB      DS    A
         LTORG
         DS    0D
AIDCODE  DC    AL2(99,13,14,15,16,17,18,19)
         DC    AL2(20,21,22,23,24,-3,-2,99)
         DC    AL2(-4,01,02,03,04,05,06,07)
         DC    AL2(08,09,10,11,12,00,00,-1)
AIDTAB   DC    C'--  PF13PF14PF15PF16PF17PF18PF19'
         DC    C'PF20PF21PF22PF23PF24CLR PA2 ????'
         DC    C'TREQPF01PF02PF03PF04PF05PF06PF07'
         DC    C'PF08PF09PF10PF11PF12ENTRSLPABUSY'
*
         LTORG
*
BLEN     EQU   4096-*+IOSRD-4
BUFFER   DS    XL(BLEN)      Screen Buffer
*
               REGEQU
         END
