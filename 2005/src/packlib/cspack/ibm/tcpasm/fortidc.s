*
* $Id: fortidc.s,v 1.1.1.1 1996/03/08 15:44:22 mclareni Exp $
*
* $Log: fortidc.s,v $
* Revision 1.1.1.1  1996/03/08 15:44:22  mclareni
* Cspack
*
*
#if defined(CERNLIB_IBMMVS)
         MACRO
         REGISTER
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
         MEND
         MACRO
&NAME    SCHL  &RET=,&MODE=
&NAME    L     13,4(13)            RELOAD SA-REG
         AIF   ('&RET' NE '').RETCODE
         LM    14,12,12(13)        RELOAD REGISTERS
         BR    14                  RETURN TO CALLER
         MEXIT
.RETCODE AIF   ('&RET'(1,1) EQ '(').REGIST
         AIF   (T'&RET NE 'N').ADDRESS
         LA    15,&RET             LOAD REG 15 WITH RET CODE
         AGO   .FURTHER
.REGIST  AIF   ('&RET(1)' EQ '15').FURTHER
         LR    15,&RET(1)          LOAD REG 15 WITH SPECIF. REGISTER
         AGO   .FURTHER
.ADDRESS L     15,&RET             LOAD REG 15 FROM STORAGE
.FURTHER LM    0,12,20(13)         RESTORE REG 0 TO 12
         L     14,12(13)           RESTORE REG 14
         BR    14                  RETURN TO CALLER
         MEND
FORTIDC  CSECT
**********************************************************************
*
*       INVOZIERT IDCAMS
*
*       AUFRUF:     CALL FORTIDC(INLINE,OUTLINE,OUTLGTH,RET)
*
*       INLINE      ENTHAELT DIE INPUT-ZEILE FUER IDCAMS (LAENGE 80)
*                   (NUR EINE ZEILE ERLAUBT).
*
*       OUTLINE     ENTHAELT DIE OUTPUT-ZEILE AUS IDCAMS (LAENGE 133)
*                   (NUR EINE ZEILE ERLAUBT).
*
*       OUTLGTH     ENTHAELT DIE LAENGE DER OUTPUT-ZEILE (FULLWORD)
*
*       RET         ENTHAELT DEN RETURN-CODE VON IDCAMS  (FULLWORD)
*                    =         0  NORMALERWEISE
*                    = MESSAGENR  BEI IDCAMS MESSAGES
*                    =     -9999  BEI EOF
*
*       NACH EOF SIND FORTIDC UND IDCAMS WIEDER JUNGFRAEULICH
*       UND KOENNEN MIT DER NAECHSTEN INPUT-KARTE GERUFEN WERDEN.
*       DER INPUT AUS INLINE WIRD NUR BEIM ALLERERSTEN AUFRUF VON
*       FORTIDC UND BEIM ERSTEN AUFRUF NACH RET = -9999 GELESEN.
*
*       IDCAMS LIEST INPUT VOM PROGRAMM (WIE GESCHILDERT),
*       SYSPRINT WIRD INS PROGRAMM UMGELEITET.
*
*                                   (C) P.KUHLMANN, DESY, 8.2.91
**********************************************************************
         PRINT NOGEN
         REGISTER
         USING *,R12
         STM   R14,R12,12(R13) SAVE REGISTERS
         LR    R12,R15         SET UP BASE REGISTER
         LA    R3,SAVE1        NEW SAVE AREA
         ST    R3,R8(R13)      SAVE ADDRESS OF NEW SAVE AREA
         ST    R13,R4(R3)      SAVE ADDRESS OF OLD SAVE AREA
         LR    R13,R3          NEW SAVE AREA
*
         L     R2,0(R1)        A(INPUT LINE)
         ST    R2,INCARD       STORE INTO IDCAMS INPUT PARAMETER
         L     R2,4(R1)        A(OUTPUT-LINE)
         ST    R2,ALINE        SAVE ADDRESS
         L     R2,8(R1)        A(LENGTH)
         ST    R2,ALGTH        SAVE ADDRESS
         L     R2,12(R1)       A(RETURNCODE)
         ST    R2,ARET         SAVE ADDRESS
FIRST    BC    0,NEXT          FIRST TIME SWITCH
         OI    FIRST+1,X'F0'   INDICATE NOT FIRST TIME
***************************************************************
*                                                             *
*      INVOKE ACCESS METHOD SERVICES                          *
*      SPECIFY I/O-ROUTINES FOR SYSIN AND SYSPRINT            *
*                                                             *
***************************************************************
         LOAD  EP=IDCAMS                  LOAD IDCAMS
         LR    R15,R0                     GET A(IDCAMS)
         CALL  (15),(OPT,OPT,OPT,AIOL),VL CALL IDCAMS
*                                         RETURN FROM IDCAMS
         NI    FIRST+1,X'0F'              RESET FIRST TIME SWITCH
         NI    GETIN+1,X'0F'              RESET FIRST TIME SWITCH
         LH    R1,=H'-9999'               INDICATE END OF IDCAMS
         L     R2,ARET                    GET ADDRESS OF RETURNCODE
         ST    R1,0(R2)                   SET RETURN CODE
         B     EXIT0                      RETURN TO CALLER
*
         DS    0F                IDCAMS PARAMETER LIST:
OPT      DC    H'0'              NO IDCAMS OPTIONS
AIOL     DC    F'2'              2 ENTRIES IN IO LIST
         DC    A(DDSYSIN)        SYSIN
         DC    A(GETC)           ROUTINE TO SATISFY SYSIN REQUESTS
         DC    A(0)
         DC    A(DDSYSPRT)       SYSPRINT
         DC    A(PUTC)           ROUTINE TO SATISFY SYSPRINT REQUESTS
         DC    A(0)
DDSYSIN  DC    CL10'DDSYSIN'
DDSYSPRT DC    CL10'DDSYSPRINT'
SAVE     DS    18F               SAVE AREA WHEN CALLED FROM IDCAMS
SAVE1    DS    18F               SAVE AREA WHEN CALLED FROM CALLER
***************************************************************
*                                                             *
*                  INPUT ROUTINE                              *
*         TO SUBMIT INPUT CARDS TO AMS                        *
*                                                             *
***************************************************************
GETC     STM   R14,R12,12(R13)     SAVE REGISTERS ON CALL FROM IDCAMS
         LR    R12,R15
         LA    R15,GETC-FORTIDC
         SR    R12,R15             SET UP BASE REGISTER
         LA    R3,SAVE             NEW SAVE AREA ON CALL FROM IDCAMS
         ST    R3,R8(R13)          SAVE NEW SAVE AREA
         ST    R13,R4(R3)          SAVE OLD SAVE AREA
         LR    R13,R3              NEW SAVE AREA
*
         L     R2,4(R1)            A(IOFLAGS)
         XR    R3,R3
         IC    R3,0(R2)            GET KIND OF REQUEST FROM IDCAMS
         B     LISTIN(R3)          BRANCH RESPECTIVELY
LISTIN   B     OPENIN              OPEN FOR INPUT
         B     CLOSEIN             CLOSE INPUT
         B     GETIN               GET INPUT
         B     PUTIN               PUT INPUT (SHOULD NEVER OCCUR)
*
OPENIN   TM    1(R2),X'A0'          OPEN FOR INPUT WITH DDNAME ?
         BNO   ABEND
         L     R2,8(R1)             A(DDNAME)
         CLC   0(8,R2),DDSYSIN+2    SYSIN ?
         BNE   ABEND
         B     EXIT0                YES, RETURN TO IDCAMS
*
GETIN    BC    0,EOF                FIRST TIME SWITCH
         OI    GETIN+1,X'F0'        INDICATE NOT FIRST TIME
         L     R2,=A(INCARD)        CURRENT INPUT CARD
         L     R3,8(R1)
         MVC   0(8,R3),0(R2)        DATA ADDRESS AND LENGTH TO IDCAMS
         B     EXIT0                RETURN TO IDCAMS
EOF      SCHL  RET=4                INDICATE END OF INPUT TO IDCAMS
INCARD   DS    F
         DC    F'80'
***************************************************************
*                                                             *
*                 OUTPUT ROUTINE                              *
*         TO COLLECT MESSAGES FROM AMS                        *
*                                                             *
***************************************************************
PUTC     STM   R14,R12,12(R13)      SAVE REGISTERS
         LR    R12,R15
         LA    R15,PUTC-FORTIDC
         SR    R12,R15              SET UP BASE REGISTER
         LA    R3,SAVE              NEW SAVE AREA ON CALL FROM IDCAMS
         ST    R3,R8(R13)           SAVE NEW SAVE AREA
         ST    R13,R4(R3)           SAVE OLD SAVE AREA
         LR    R13,R3               NEW SAVE AREA
*
         LR    R8,R1                SAVE PARAMETER REGISTER
         L     R2,4(R8)             A(IOFLAGS)
         XR    R3,R3
         IC    R3,0(R2)             GET KIND OF REQUEST FROM IDCAMS
         LH    R6,2(R2)             GET MESSAGE NR
         B     LISTOUT(R3)          BRANCH RESPECTIVELY
LISTOUT  B     OPENOUT              OPEN FOR OUTPUT
         B     CLOSEOUT             CLOSE OUTPUT
         B     GETOUT               GET OUTPUT (SHOULD NEVER OCCUR)
         B     PUTOUT               PUT OUTPUT
*
OPENOUT  TM    1(R2),X'60'          OPEN FOR OUTPUT WITH DDNAME ?
         BNO   ABEND
         L     R2,8(R8)             A(DSNAME)
         CLC   0(8,R2),DDSYSPRT+2   SYSPRINT
         BNE   ABEND
         B     EXIT0
*
PUTOUT   L     R2,8(R8)             PUT LIST
         L     R3,0(R2)             A(OUTPUT LINE)
         L     R4,4(R2)             LENGTH OF PUTPUT LINE
         L     R5,ALGTH
         ST    R4,0(R5)             SET OUTPUT LGTH
         BCTR  R4,0
         L     R5,ALINE             RESTORE OUTPUT POINTER ADDRESS
         EX    R4,MOVLN             SET OUTPUT LINE
         L     R2,ARET              RESTORE ADDRESS OF RETURN CODE
         ST    R6,0(R2)             SET MESSAGE NUMBER
         LA    R13,SAVE1            RESTORE CALLERS SAVE ROUTINE
         B     EXIT0                EXIT TO CALLER
MOVLN    MVC   0(1,R5),0(R3)
*
NEXT     LA    R13,SAVE             RETURN FROM CALLER
CLOSEIN  EQU   *                    CLOSE FOR INPUT
CLOSEOUT EQU   *                    CLOSE FOR OUTPUT
EXIT0    SCHL  RET=0                RETURN TO IDCAMS
*
PUTIN    EQU   *                    PUT INPUT
GETOUT   EQU   *                    GET OUTPUT
ABEND    ABEND 1,DUMP
*
ALINE    DS    F                    A(OUTPUT LINE)
ALGTH    DS    F                    A(OUTPUT LENGTH)
ARET     DS    F                    A(RETURN CODE)
*
         END   FORTIDC
#endif
