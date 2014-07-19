*
* $Id: linepg.s,v 1.1.1.1 1996/02/15 17:53:19 mclareni Exp $
*
* $Log: linepg.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:19  mclareni
* Kernlib
*
*
LINEPG   CSECT
*
* CERN PROGLIB# Z033    LINEPG          .VERSION KERNIBM  2.12  850529
*
* THIS ROUTINE RETURNS THE VALUE OF THE JES (JOB ENTRY SUBSYSTEM)
* LINES PER PAGE PARAMETER. IT USES A CERN WRITTEN SVC NUMBER 250
* ($XGET) AND A SHADOW JES JCT DSECT (MACRO $XJES). IT WILL NOT
* WORK OUTSIDE OF CERN. TO ASSEMBLE AT CERN REQUIRES 800K REGION PLUS
*
*   ALB1='SYS2.CRNMAC',
*   ALB2='SYS1.MACLIB',
*   ALB3='SYS1.AMODGEN,VOL=REF=PASSWORD'
*
**************************************************
*
*    A. BERGLUND / CERN      DATE: MARCH 1981
*
**************************************************
*
*
*         CALL FROM FORTRAN LIKE
*
*            CALL LINEPG(FTN STREAM NO OR 'DDNAME' ,IARRAY)
*                  IARRAY(1)= 'NORMAL' LINES PER PAGE
*                         2    PHYSICAL NR LINES ON THE PAPER
*                         3    HW IMPOSED TOP MARGIN
*                         4    HW IMPOSED BOTTOM MARGIN
*                         5    EXTRA TOP MARGIN AT PAGE THROW
*                         6    EXTRA BOTTOM MARGIN
*
*               LINES PER PAGE IS I*4
*
*
*-----
R0       EQU   0
R1       EQU   1
JCTF     EQU   2
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
*---------
*
*  P R O L O G U E
*
         USING LINEPG,R15
         B     SAVEREGS
         DC    AL1(7),CL7'LINEPG'      IDENTIFIER
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
         LR    R12,R1         R12 -> ARGUMENT LIST
*---------
*
*       GET THE INTERESTING PARTS OF THE JCT
*       USING THE CERN WRITTEN SVC 250
*
        $XGET  AREA=FJCT,FIELDS=(J2)
         LTR   R15,R15
         BNZ   ERROR
         LA    JCTF,FJCT      ADDRESS OF FILED
         USING XJ2DSECT,JCTF
         SR    R3,R3           CLEAR R3
         IC    R3,XCTLINCT     GET LINECOUNT BYTE
         LTR   R3,R3         CHECK IF ZERO (IE DEFAULT)
         BZ    DEFAULT
         L     R4,4(R12)     ADDRESS OF 2ND ARG
         ST    R3,0(R4)        STORE RESULT
         ST    R3,4(R4)       STORE AS 2ND ELEMENT
         MVC   8(16,R4),ZERO  MOVE 4 FULLWORDS OF ZERO
         B     EPILOGUE      RETURN
ZERO     DC    4F'0'
*
*       FOR DEFAULT YOU HAVE TO GET THE PHYSICAL
*       CAPABILITY OF THE DEVICE
*
DEFAULT  EQU   *
*
*    SEE FIRST IF IT IS R4 AT CERN
*    I.E. GOING TO CDC
*
         CLC   XCTPROUT,=X'1104' NODE 11 RMT 4
         BE    CDC               IF SO GO AND SET UP CDC
         SR    10,10
         L     2,0(R12)            ADDRESS OF 1ST PARAMETER
         SR    3,3
         IC    3,0(2)            FIRST BYTE - 0 IF INTEGER
         LTR   3,3               SEE IF ZERO
         BZ    CONCAT            CONSTRUCT DDNAME
         LD    2,0(2)            DDNAME IN FLPR 2/3
         B     CVTF
CONCAT   L     3,0(2)            I*4 STREAM NO
         CVD   3,TEMP8
         MVC   EDITAREA(L'PATTERN1),PATTERN1
         ED    EDITAREA(L'PATTERN1),TEMP8+6
         MVC   FTFILE+2(2),EDITAREA+1
         LD    2,FTFILE            DDNAME INTO FLPR 2/3
CVTF     L     11,16             ADDRESS OF CVT
         L     11,0(11)          ADDRESS OF TCB WORDS
         L     11,4(11)          ADDRESS OF CURRENT TCB
         L     11,12(11)         ADDRESS OF USER'S TIOT
         LA    11,24(11)         ADDRESS OF FIRST DD ENTRY IN TIOT
*
NEXTDD   IC    10,0(11)          BYTE COUNT LENGTH OF DD ENTRY
         LTR   10,10             SEE IF ZERO (END OF TIOT)
         BZ    ENDTIOT
         CD    2,4(11)           COMPARE DDNAME
         BE    FOUND
         AR    11,10             SKIP THIS DD ENTRY
         B     NEXTDD
*
*               DDNAME WAS NOT IN TIOT DDNAME LIST
*
ENDTIOT  EQU  *                  CODE FOR DDNAME MISSING
SET56    EQU  *      VARIOUS BRANCHES TO HERE TO SET DEFAULT 56
*                    MOSTLY DUE TO ERRORS OF ONE SORT OR OTHER
         LA   R3,NORMAL          SET IT TO DEFAULT 56
         L    R4,4(R12)          ADDRESS OF SECOND ARGUMENT
         MVC   0(24,R4),0(R3)    MOVE 6 FULLWORDS
         B    EPILOGUE           AND RETURN
FOUND    L     6,11(11)  ADDRESS OF JFCB-10(HEX)
         LA    6,16(6)     ADDRESS OF JCFB
         USING INFMJFCB,6
*
*                 WE NEED THE SIOT FOR SYSOUT CLASS
          L     11,16    CVT
          L     11,0(11)    TCB WORDS
          L     11,4(11)    CURRENT TCB
          L     11,180(11)  JSCB
          L     11,260(11)  JCT-10 HEX
          L     11,47(11)   SCT-10 HEX
LOOP      L     10,35(11)   NEXT SCT-10
          LTR   10,10       TEST IF ZERO
          BZ    LAST
          LR    11,10
          B     LOOP
LAST      L     11,27(11)    START OF SIOT QUEUE OF CURRENT STEP
*                           THEN LOOK FOR THE SIOT FOR PROPER
          LA    11,16(11)   UPDATE TO START OF SIOT
          CD    2,4(11)    COMPARE FIRST ONE IN QUEUE
*                           DDNAME
          BNE   SIOTLP      IF NOT GO INTO LOOP
          B     SIOTFND
SIOTLP    L     8,152(11)  NEXT SIOT
          LTR   8,8         END OF QUEUE?
          BZ    NOSIOT
          LR    11,8
          CD    2,4(11)     COMPARE DDNAMES
          BE    SIOTFND
          B     SIOTLP
NOSIOT    EQU   *         TAKE THE DEFAULT
          B     SET56         SET DEFAULT AND RETURN
SIOTFND   EQU   *
          USING INDMSIOT,11
         TM    JFCBTSDM,JFCSDS    TEST IF SYSIN OR SYSOUT
         BZ    SET56               IF NOT SET DEFAULT
         CLI   SCTOUTPN,C'W'      SEE IF SYSOUT=W
         BE    CLASSW
         CLI   SCTOUTPN,C'F'      SEE IF SYSOUT=F
         BE    CLASSF
         CLI   SCTOUTPN,C'C'      SEE IF SYSOUT=C
         BE    CLASSC
*
*       NOT ON THE 3800
*
         B     SET56              SO SET IT TO 56
*
CLASSW   EQU   *
         CLC   JFCFCBID(4),=C'W6  '
         BNE   W2
         LA    R3,W66             66 LINES PER PAGE
         B     SETR3
W2       CLC   JFCFCBID(4),=C'W8  '
         BNE   W3
         LA    R3,W88             88 LINES PER PAGE
         B     SETR3
W3       CLC   JFCFCBID(4),=C'W12 '
         BNE   SET56             IF USER GOOFED => 56
         LA    R3,W132           132 LINES PER PAGE
         B     SETR3
CLASSF   EQU   *
         CLC   JFCFCBID(4),=C'N6  '
         BNE   F2
         LA    R3,F44             44 LINES PER PAGE
         B     SETR3
F2       CLC   JFCFCBID(4),=C'N8  '
         BNE   F3
         LA    R3,F58             58 LINES PER PAGE
         B    SETR3
F3       CLC   JFCFCBID(4),=C'N12 '
         BNE   SET56              IF USER GOOFED => 56
         LA    R3,F88             88 LINES PER PAGE
         B     SETR3
CLASSC   EQU   *
         LA    R3,CNORMAL         XEROX DEFAULT VALUES
         B     SETR3
CDC      EQU   *
         LA    R3,CDCR            CDC 'RIOS' VALUES
SETR3    EQU   *
         L     R4,4(R12)          ADDRESS OF SECOND ARGUMENT
         MVC   0(24,R4),0(R3)    MOVE 6 FULLWORDS
         B     EPILOGUE
ERROR    EQU   *
         L     R4,4(R12)          ADDRESS OF 2ND ARGUMENT
         ST    R15,4(R4)          2ND ELEMENT OF ARRAY
         LA    R15,999(0)
         ST    R15,0(R4)          SET 'ERROR VALUE'
         B     EPILOGUE
*  E P I L O G U E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         OI    12(R13),X'FF'
         BR    R14
*---------
*  D A T A   A R E A S
*
TEMP8    DS    D
EDITAREA DS    3C
PATTERN1 DC    X'212020'
FTFILE   DC    CL8'FT??F001'
         LTORG
NORMAL   DC    F'56'          MAX LINES PER PAGE
         DC    F'64'          PHYSICAL SIZE OF PAGE
         DC    F'0'           IMPOSED TOP MARGIN
         DC    F'0'           IMPOSED BOTTOM MARGIN
         DC    F'2'           EXTRA TOP MARGIN (AT PAGE EJECT)
         DC    F'6'           EXTRA BOTTOM MARGIN
W66      DC    F'66'          MAX LINES PER PAGE
         DC    F'72'          PHYSICAL SIZE OF PAGE
         DC    F'3'           IMPOSED TOP MARGIN
         DC    F'3'           IMPOSED BOTTOM MARGIN
         DC    F'0'           EXTRA TOP MARGIN (AT PAGE EJECT)
         DC    F'0'           EXTRA BOTTOM MARGIN
W88      DC    F'88'          MAX LINES PER PAGE
         DC    F'96'          PHYSICAL SIZE OF PAGE
         DC    F'4'           IMPOSED TOP MARGIN
         DC    F'4'           IMPOSED BOTTOM MARGIN
         DC    F'0'           EXTRA TOP MARGIN (AT PAGE EJECT)
         DC    F'0'           EXTRA BOTTOM MARGIN
W132     DC    F'132'         MAX LINES PER PAGE
         DC    F'144'         PHYSICAL SIZE OF PAGE
         DC    F'6'           IMPOSED TOP MARGIN
         DC    F'6'           IMPOSED BOTTOM MARGIN
         DC    F'0'           EXTRA TOP MARGIN (AT PAGE EJECT)
         DC    F'0'           EXTRA BOTTOM MARGIN
F44      DC    F'44'          MAX LINES PER PAGE
         DC    F'50'          PHYSICAL SIZE OF PAGE
         DC    F'4'           IMPOSED TOP MARGIN
         DC    F'2'           IMPOSED BOTTOM MARGIN
         DC    F'0'           EXTRA TOP MARGIN (AT PAGE EJECT)
         DC    F'0'           EXTRA BOTTOM MARGIN
F58      DC    F'58'          MAX LINES PER PAGE
         DC    F'67'          PHYSICAL SIZE OF PAGE
         DC    F'6'           IMPOSED TOP MARGIN
         DC    F'3'           IMPOSED BOTTOM MARGIN
         DC    F'0'           EXTRA TOP MARGIN (AT PAGE EJECT)
         DC    F'0'           EXTRA BOTTOM MARGIN
F88      DC    F'88'          MAX LINES PER PAGE
         DC    F'100'         PHYSICAL SIZE OF PAGE
         DC    F'8'           IMPOSED TOP MARGIN
         DC    F'4'           IMPOSED BOTTOM MARGIN
         DC    F'0'           EXTRA TOP MARGIN (AT PAGE EJECT)
         DC    F'0'           EXTRA BOTTOM MARGIN
CNORMAL  DC    F'56'          MAX LINES PER PAGE
         DC    F'58'          PHYSICAL SIZE OF PAGE
         DC    F'1'           IMPOSED TOP MARGIN
         DC    F'1'           IMPOSED BOTTOM MARGIN
         DC    F'0'           EXTRA TOP MARGIN (AT PAGE EJECT)
         DC    F'0'           EXTRA BOTTOM MARGIN
CDCR     DC    F'62'          MAX LINES PER PAGE
         DC    F'64'          PHYSICAL SIZE OF PAGE
         DC    F'0'           IMPOSED TOP MARGIN
         DC    F'0'           IMPOSED BOTTOM MARGIN
         DC    F'2'           EXTRA TOP MARGIN (AT PAGE EJECT)
         DC    F'0'           EXTRA BOTTOM MARGIN
FJCT     DS    XL(XJ2L)       JCT FIELDS
*---------
         PRINT NOGEN
         TITLE  'DSECTS'
JFCB     DSECT
         IEFJFCBN
SIOT     DSECT
         IEFASIOT
CVT      DSECT
         CVT   ,
*--------- ,
         IKJTCB ,
*--------- ,
         IEZJSCB ,
         $XJES FIELDS=(J2)
*        EJECT ,
*        CSECT ,    (JES DSECT MACROS ALWAYS REVERT TO A CSECT)!
*        $SSIB ,
*        $BUFFER ,    (NEEDED FOR $JCT EXPANSION)
*        $JCT  ,
*        $SJB  ,
*
*--
         END
