*
* $Id: iospack.s,v 1.1.1.1 1996/02/15 17:51:54 mclareni Exp $
*
* $Log: iospack.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:54  mclareni
* Kernlib
*
*
*
* CERN PROGLIB# Z304    IOSPACK         .VERSION KERNIBM  2.20  870601
* ORIG. 03/07/85 E.PEROTTO
*
*   GENERAL PURPOSE FULL SCREEN INTERFACE PACKAGE FOR IBM VM/CMS
*
         TITLE 'MACROS'
*--------------------------------------------------------------------
               MACRO
         PAGE
         PUNCH '$SPB'              TELL LOADER TO START AT NEXT PAGE
               MEND
*--------------------------------------------------------------------
               MACRO
         DISPW &ADR,&BYTES=,&LINE=,&CANCEL=NO,&ERASE=NO,&DISPLAY=NO
         LA    R1,DISPCCW              GET CCW ADDRESS IN R1
         STCM  &ADR(1),B'0111',1(R1)   STORE BUFFER ADDRESS
               AIF ('&LINE' EQ '').NOLINE
         STC   &LINE(1),5(R1)          STORE LINE NUMBER
.NOLINE        AIF ('&ERASE' NE 'YES').NOERASE
         OI    5(R1),X'80'             SET 'MORE...' FLAG
.NOERASE       AIF ('&CANCEL' NE 'YES').NOCANCL
         OI    5(R1),X'FF'             SET CLEAR FLAG
.NOCANCL STH   &BYTES(1),6(R1)         STORE BYTE COUNT
               AIF ('&DISPLAY' EQ 'YES').DISPL
         B     DISPLAY
               MEXIT
.DISPL         ANOP
DISPLAY  LA    R2,9               GET CONSOLE ADDRESS
         DIAG  R1,R2,X'58'        WRITE TO SCREEN
         B     DISPCCW+8
*
DISPCCW  CCW   X'19',0,SILI,0     DISPLAY CCW
SILI     EQU   X'20'              SUPPRESS INCORRECT LENGTH INDICATION
*
               MEND
*--------------------------------------------------------------------
*--            PAGE               LOAD ON A FULL PAGE BOUNDARY
         TITLE 'GENERAL PURPOSE FULL-SCREEN INTERFACE ROUTINES'
*--------------------------------------------------------------------
*        ROUTINE TO WRITE A LINE ON FULL-SCREEN TERMINAL
*--------------------------------------------------------------------
*
*    IRC = IOSPACK(LINE,LENGTH,LINENO)
*
*       IRC  =  INTEGER RETURN CODE: 0 = ALL OK
*                                   -1 = TERMINAL BUSY
*                                   -2 = UNRECOVERABLE I/O ERROR
*      LINE  =  CHARACTER ARRAY OR HOLLERITH CONSTANT
*    LENGTH  =  LENGTH OF STRING IN BYTES (MAY BE 0)
*    LINENO >0: SCREEN LINE NUMBER  (ABSOLUTE)
*           =0: NEXT AVAILABLE LINE (OUTPUT AREA = 1 TO LAST-2)
*           <0: OVERWRITE PREVIOUS LINES (UP TO TOP LINE, ELSE CLEAR)
*           -1: OVERWRITE LAST LINE
*           OUT >LAST: 1ST LINE ON NEXT SCREEN AFTER MORE...
*           OUT <0:    CLEAR SCREEN
*           OUT = 98:  LAST LINE-2 (NORMALLY PROTECTED)
*           OUT = 99:  1ST INPUT LINE (LAST LINE-1)
*           OUT =100:  2ND INPUT LINE (LAST SCREEN LINE)
*           OUT >100:  1ST LINE ON NEXT SCREEN AFTER MORE...
*
* ALTERNATE ENTRY POINTS:
*     IRC = IOSPUT(LINE,LENGTH,LINENO)        EQ. TO IOSPACK
*     IRC = IOSSIZ(LINES)             LINES = ADDRESSABLE OUTPUT LINES
*     IRC = IOSCLR()                          CLEARS THE SCREEN
*     LINENO = IOSLIN()              LINENO = NEXT FREE LINE ON SCREEN
*
* AUTHOR: ELIA PEROTTO - CERN DD DIVISION - TEL. 7672303
*                      - CNUCE/CNR - PISA - TEL. 593246
* VERSION 2.0 - 25 JULY 1985
*
IOSPACK  CSECT
#if defined(CERNLIB_QMIBMXA)
IOSPACK  AMODE ANY
IOSPACK  RMODE ANY
#endif
               ENTRY IOSPUT,IOSSIZ,IOSCLR,IOSLIN
IOSPUT   EQU   *
&DT      SETC  'IOSPACK &SYSDATE&SYSTIME'
         SAVE  (14,12),,&DT
         LR    R12,R15
               USING IOSPACK,R12
IOSTART  LR    R10,R1             SAVE ARGS POINTER
               HNDINT SET,(CON1,NETINT,009,ASAP) START INTERRUPT HANDL.
GATE     B     INIT               FETCH REAL CONSOLE CHARACTERISTICS
*
         LM    R3,R5,0(R10)       LOAD ARGS ADDRESSES
         L     R4,0(,R4)          R4 = BUFFER LENGTH
         LA    R2,0(,R3)          R2 = BUFFER ADDRESS
         LTR   R4,R4              IS IT A NULL LINE?
         BH    NOTNULL            NO, FINE
         LA    R2,BLANK           YES, 0 YELDS A CHANNEL PROGRAM CHECK
         LA    R4,1               MUST SEND AT LEAST ONE BYTE
NOTNULL  EQU   *
         ICM   R5,B'1111',0(R5)   LINE NUMBER ON 3270 SCREEN (1 TO 24)
         BL    BACKLINE           OVERWRITE A LINE
         BZ    NEXTLINE           NEXT AVAILABLE LINE
         BCTR  R5,0               FROM (1 TO 24) TO (0 TO 23)
         BAL   R14,CHECKLIN       DOES IT FIT INTO SCREEN?
         BNH   ADDRLINE           YES
         SH    R5,=H'99'          IS IT <99 (100)?
         BH    ERASE              NO, SHOW IT ON A NEW SCREEN
         A     R5,LASTLINE        GET ABSOLUTE SCREEN LINE NUMBER
         BM    ERASE              NEGATIVE, SHOW IT ON A NEW SCREEN
         B     ADDRLINE           WRITE IT THERE
*
BACKLINE AH    R5,CURLINE         GO BACK -N LINES
         BL    CLEAR              OUT OF SCREEN
         STH   R5,CURLINE         UPDATE CURRENT LINE POINTER
NEXTLINE LH    R5,CURLINE         GET CURRENT LINE POINTER
         BAL   R14,CHECKLIN       CHECK LINE LENGTH
         BH    ERASE              SCREEN OVERFLOW, GO TO NEXT
BLANK    STH   R6,CURLINE         UPDATE CURRENT LINE POINTER
ADDRLINE LH    R0,=H'-1'          ENTRY FOR DIRECT LINE WRITE (1 TO 23)
         TS    FLAG               IS THE PATH CLEAR?
         BNZ   ERROR              NO, SIGNAL ERROR TO CALLER
         STH   R5,DISPLINE        SAVE HERE FOR TRACE
               DISPW (R2),BYTES=(R4),LINE=(R5),DISPLAY=YES
         LH    R0,=H'-2'          ERROR CODE FOR I/O ERROR
         BNZ   ERROR              SOMETHING WENT WRONG IN THE DIAGNOSE
               WAITD CON1    WAIT FOR CONSOLE I/O (WAITT DOES NOT WORK)
RETURN   XR    R0,R0              SIGNAL NO ERRORS
ERROR    ST    R0,20(R13)         SAVE IT ON CALLER'S R0
               HNDINT CLR,CON1    STOP INTERRUPT HANDLING
         MVI   FLAG,0             RESET FLAG
               RETURN (14,12)     RETURN TO FORTRAN CALLER
*
*--------------------------------------------------------------------
*        CHECK IF LINE FITS INTO SCREEN
*--------------------------------------------------------------------
*
* IN:    R4 =  BUFFER LENGTH
*        R5 =  SCREEN LINE NUMBER (0 TO 23)
*
* OUT:   R6 =  NEXT AVAILABLE LINE NUMBER
*        R7 =  BUFFER LENGTH IN LINES
*        CC SET
                             MNOTE ''
CHECKLIN LR    R7,R4              COPY BUFFER LENGTH
         BCTR  R7,0               NOT TO SKIP A LINE IF 80 BYTES
         XR    R6,R6              CLEAR FOR DIVIDE
         D     R6,LINELEN         YELDS FULL SCREEN LINES
         LA    R6,1(R7,R5)        NEXT AVAILABLE LINE
         C     R6,LASTOUTL        MORE THAN LAST OUTPUT LINE
         BR    R14
*
*--------------------------------------------------------------------
*        PUT SCREEN INTO MORE... STATE
*--------------------------------------------------------------------
ERASE    EQU   *
         LA    R7,1(R7)           ROUND TO NEXT FULL LINE
         STH   R7,CURLINE         UPDATE NEXT AVAILABLE LINE
         XR    R5,R5              SET LINE 0
         TS    FLAG
         BNZ   ERROR
               DISPW (R2),BYTES=(R4),LINE=(R5),ERASE=YES
*
*--------------------------------------------------------------------
*        CLEAR SCREEN
*--------------------------------------------------------------------
CLEAR    EQU   *                  OUT OF SCREEN
         XR    R1,R1              UPDATE NEXT AVAILABLE LINE
         STH   R1,CURLINE
         TS    FLAG
         BNZ   ERROR
               DISPW (R2),BYTES=(R4),CANCEL=YES
*
*--------------------------------------------------------------------
*        INITIALIZE DEVICE CHARACTERISTICS
*--------------------------------------------------------------------
INIT     BAL   R14,GETMODEL
         B     GATE
*
GETMODEL XR    R0,R0              CLEAR ERROR CODE
         LA    R4,9               CONSOLE ADDRESS
         DIAG  R4,R5,X'24'        GET CONSOLE INFORMATION
         STM   R4,R6,CONSINFO     SAVE IT HERE
         MVI   GATE+1,0           NOP THE BRANCH
         LA    R1,24              MODEL 2 HAS 24 LINES
         CLI   CONSINFO+10,2      IS IT A MODEL 2?
         BE    MODELX             YES IT IS
         LA    R1,27              DEFAULT MODEL 5 LINES (27)
         LA    R4,DEVINFO         BUFFER ADDRESS
         LA    R5,9               CONSOLE ADDRESS
         LA    R6,8               BUFFER LENGTH
         DIAG  R4,R6,X'8C'        GET MODEL 5 SCREEN DIMENSIONS
         LTR   R0,R5              ANY ERROR?
         BNZ   MODELX             YES
         LH    R1,DEVINFO+2       GET SCREEN WIDTH
         ST    R1,LINELEN
         LH    R1,DEVINFO+4       GET SCREEN HEIGHT
MODELX   LR    R5,R1
         BCTR  R5,0               PUT IT IN DISPLACEMENT FORM
         ST    R5,LASTLINE        LAST SCREEN LINE
         BCTR  R5,0               PROTECT THE TWO...
         BCTR  R5,0               ...INPUT LINES
         ST    R5,LASTOUTL        LAST OUTPUT LINE
         STH   R5,CURLINE         NEXT SEQ. WRITE WILL GO INTO MORE...
         BR    R14                BACK TO NORMAL OPERATION
*
               USING IOSSIZ,R15
IOSSIZ   SAVE  (14,12)
         L     R12,=A(IOSPACK)
               DROP R15
         L     R2,0(R1)
         BAL   R14,GETMODEL       GET 3270 MODEL INFORMATION
         ST    R1,0(R2)           STORE INTO USER'S VARIABLE
         ST    R0,20(R13)
               RETURN (14,12)
*
               USING IOSLIN,R15
IOSLIN   SAVE  14
         LH    R0,CURLINE
               RETURN 14
               DROP R15
*
               USING IOSCLR,R15
IOSCLR   SAVE  (14,12)
         L     R12,=A(IOSPACK)
               DROP R15
         LA    R1,CLRPLIST
         B     IOSTART
*
*--------------------------------------------------------------------
*        DEVICE I/O INTERRUPT HANDLER
*--------------------------------------------------------------------
NETINT   EQU   *
               USING *,R15
         CLI   FLAG,X'FF'         IS IT OUR START I/O?
         BNE   INTPASS            NO
         CH    R4,=H'9'           IS IT CONSOLE?
         BNE   INTPASS            NO
         STM   R2,R3,CSW          SAVE CSW
         TM    CSW+4,DE           LAST INTERRUPT?
         BZ    INTEXIT            NO, WAIT FOR NEXT
         MVI   FLAG,0             YES, CLEAR FLAG TO SIGNAL COMPLETION
INTPASS  XR    R15,R15            DISPATCH MAIN TASK
INTEXIT  BR    R14                RETURN TO CMS INTERRUPT HANDLER
*
FLAG     DC    X'00'              SYNCHRONIZATION FLAG (0 = FREE)
CSW      DS    D                  SAVED FROM LAST INTERRUPT
DE       EQU   X'04'         DEVICE END BIT IN CSW
*
DEVINFO  DS    0D            BUFFER FOR DIAGNOSE 24
CONSINFO DS    3F            BUFFER FOR DIAGNOSE 8C
DISPLINE DC    H'0'          LAST DISPLAY BUFFER START LINE (0 TO 23)
CURLINE  DC    H'21'         NEXT SCREEN LINE NUMBER (0 TO 23)
LASTLINE DC    F'23'         2ND INPUT LINE
LASTOUTL DC    F'21'         LEAVE LINES 22 & 23 FOR INPUT
LINELEN  DC    F'80'         DEFAULT LINE LENGTH
*
CLRCOUNT DC    F'-255'       FOR IOSCLR()
CLRPLIST DC    A(*,LINELEN,CLRCOUNT)
*
         LTORG
*
               REGEQU
         END
