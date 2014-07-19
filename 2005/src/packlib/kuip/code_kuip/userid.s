*
* $Id: userid.s,v 1.1.1.1 1996/03/08 15:32:55 mclareni Exp $
*
* $Log: userid.s,v $
* Revision 1.1.1.1  1996/03/08 15:32:55  mclareni
* Kuip
*
*
#if defined(CERNLIB_IBMMVS)
*CMZ :  2.05/02 11/05/94  09.50.52  by  Alfred Nathaniel
*-- Author :
* IBMMVS: GET RACF Userid for Prefix.
*         To compile (GSI): Use SYS1.AMODGEN also !
*   22/01/91 101231002  MEMBER NAME  USERID   (X.S)         ASSEMBLER
***********************************************************************
*                                                                     *
*                      U S E R I D                                    *
*                                                                     *
*         Written by Dietrich Moenkemeyer DESY Hamburg                *
*                                                                     *
*                      January 1991                                   *
*                                                                     *
***********************************************************************
*                                                                     *
*   This routine returns the job's or TSO session's RACF userid.
*   If the user is not RACF defined the routine returns with RC=12    *
*                                                                     *
***********************************************************************
*                                                                     *
*   Call:     CALL USERID(userid)                                     *
*                                                                     *
*   Output:   userid    8 BYTES   RACF userid                         *
*                                                                     *
*   LKED:     reentrant, 31bit                                        *
*                                                                     *
*   Retcodes: 12 user not RACF defined                                *
*                or not supplied at RACINIT                           *
*              8 inconsistent control block chain                     *
*              4 incorrect length of userid (<0 or >8)                *
*                                                                     *
***********************************************************************
*   Change activity:                                                  *
***********************************************************************
*
*
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
*
         MACRO
&NAME    ANF   &ID,&MODE=REUS,&WKSIZE=0,&SP=0,&BASE=12
         GBLC  &DY0ID,&MD
         LCLA  &A,&B,&D
         LCLC  &E,&F
&F       SETC  '&SYSNDX'
&MD      SETC  '&MODE'
* ANF VERSION 2.4 (SUPPORTS PRNT MACRO AND EQUATED WORKSIZE)
.* DIE VORIGE VERSION HEISST ANF8
         AIF   ((T'&WKSIZE NE 'N') AND (T'&WKSIZE NE 'U')).ERROR4
         AIF   (T'&SP NE 'N').ERROR5
         AIF   (&SP GT 255).ERROR6
         AIF   (T'&BASE NE 'N').ERROR7
         AIF   ((&BASE GT 12) OR (&BASE LT 4)).ERROR8
         AIF   ('&MODE' NE 'REUS').RENT
         AIF   ('&ID' EQ '').NULLID
         AIF   ('&ID' EQ '*').SPECID
&D       SETA  K'&ID
         AIF   (&D GT 8).ERROR1
&A       SETA  ((K'&ID+4)/4)*4+80
&E       SETC  '&ID'
         CNOP  0,4
&NAME    B     &A.(0,15)           BRANCH AROUND ID AND SA
         DC    AL1(&D)             LENGTH OF ID
         DC    CL&D'&E'
         AGO   .CONTA
.NULLID  ANOP
         CNOP  0,4
&NAME    B     80(0,15)            BRANCH AROUND SA
         AGO   .CONTA
.SPECID  AIF   ('&NAME' EQ '').CSECTN
&E       SETC  '&NAME'
&A       SETA  1
.CONTB   AIF   ('&E'(1,&A) EQ '&E').CONTC
&A       SETA  &A+1
         AGO   .CONTB
.CONTC   ANOP
         AIF   ('&MODE' EQ 'RENT').CONTE
&B       SETA  ((&A+4)/4)*4+80
         CNOP  0,4
&NAME    B     &B.(0,15)           BRANCH AROUND ID AND SA
         DC    AL1(&A)             LENGTH OF ID
         DC    CL&A'&E'
         AGO   .CONTA
.CSECTN  AIF   ('&SYSECT' EQ '').ERROR2
&E       SETC  '&SYSECT'
&A       SETA  1
         AGO   .CONTB
.ERROR2  MNOTE 12,'CSECT NAME NOT SPECIFIED'
         AGO   .NULLID
.CONTA   ANOP
&DY0ID   SETC  '&SYSNDX'
SAVE&F   DC    19F'0'              SAVE AREA
         STM   14,12,12(13)        SAVE CALLER'S REGISTER
         LR    &BASE,15            LOAD BASE
         LA    3,SAVE&F            ADDRESS OF NEW SAVE AREA
         ST    3,8(13)             FORWARD CHAIN
         L     3,32(13)            RESTORE REG 3
         ST    13,SAVE&F.+4        BACKWARD CHAIN
         LA    13,SAVE&F           LOAD SAVE AREA REG
         MEXIT
.ERROR1  MNOTE 12,'IDENTIFICATION FIELD TOO LONG'
         MEXIT
.RENT    ANOP
         AIF   ('&MODE' NE 'RENT').ERROR3
         AIF   ('&ID' EQ '').RNULL
         AIF   ('&ID' EQ '*').SPECID
&D       SETA  K'&ID
         AIF   (&D GT 8).ERROR1
&A       SETA  ((&D+2)/2)*2+4
&E       SETC  '&ID'
&NAME    B     &A.(0,15)           BRANCH AROUND ID
         DC    AL1(&D)             LENGTH OF ID
         DC    CL&D'&E'
         STM   14,12,12(13)        SAVE CALLER'S REGISTER
         AGO   .CONTD
.RNULL   ANOP
&NAME    STM   14,12,12(13)        SAVE CALLER'S REGISTER
         AGO   .CONTD
.CONTE   ANOP
&B       SETA  ((&A+2)/2)*2+4
&NAME    B     &B.(0,15)           BRANCH AROUND ID
         DC    AL1(&A)             LENGTH OF ID
         DC    CL&A'&E'
         STM   14,12,12(13)        SAVE CALLER'S REGISTER
.CONTD   ANOP
         LR    &BASE,15            LOAD BASE
         CNOP  0,4                 ALIGN ON FULL WORD BOUNDARY
         BAL   1,*+8               BRANCH AROUND SUBPOOL AND LENGTH
         DC    AL1(&SP)            SUBPOOL ID
         DC    AL3(&WKSIZE+88)     LENGTH VALUE
         L     0,0(0,1)            LOAD SP AND LENGTH
         SVC   10                  ISSUE GETMAIN SVC
         XC    0(88,1),0(1)        CLEAR CHAIN FIELDS AND SAVE AREA
         MVI   8(1),X'FF'          INDICATE LAST ON CHAIN
         MVC   12(4,1),*-20        SAVE LENGTH AN SUBPOOL
         MVC   0(4,1),12(13)
         LA    15,FR&F
         ST    15,12(13)
         ST    13,20(1)            BACKWARD CHAIN
         LR    15,13               SAVE OLD SAVE AREA POINTER
         LA    13,16(1)            LOAD NEW SA REGISTER
         ST    13,8(15)            FORWARD CHAIN
         LM    15,1,16(15)         RESTORE ALL REGISTER
         B     FO&F
FR&F     ST    15,16(13)           SAVE RETURN CODE
         LA    0,16                LENGTH OF PREFIX
         L     1,8(13)             SAVE OLD SA POINTER
         SR    1,0                 POINT TO PREFIX
         MVC   12(4,13),0(1)
         L     14,8(1)             LOAD 1-ST CHAIN POINTER
         L     0,12(1)             LOAD 1-ST SUBPOOL AND LENGTH
         BALR  10,0
         B     12(10)
         L     14,0(1)             LOAD CHAIN POINTER
         L     0,4(1)              LOAD SUBPOOL AND LENGTH
         LA    1,0(1)              CLEAR HIGH ORDER BYTE
         SVC   10                  ISSUE FREEMAIN SVC
         LA    1,0(14)             GET CHAIN POINTER
         C     14,36(10)           LAST IN CHAIN
         BNZ   4(10)               NO, BRANCH
         LM    14,12,12(13)        RESTORE REGISTERS AFTER FREEMAIN
         BR    14
         DC    X'FF000000'
FO&F     EQU   *
         MEXIT
.ERROR3  MNOTE 12,'MODE=&MODE INVALID'
         MEXIT
.ERROR4  MNOTE 12,'WKSIZE=&WKSIZE SHOULD BE SELF-DEFINING TERM OR NAME *
               FIELD OF AN ''EQU'''
         MEXIT
.ERROR5  MNOTE 12,'SP=&SP SHOULD BE A SELF DEFINING TERM'
         MEXIT
.ERROR6  MNOTE 12,'SP=&SP GREATER 255 SPECIFIED'
         MEXIT
.ERROR7  MNOTE 12,'BASE=&BASE SHOULD BE A SELF DEFINING TERM'
         MEXIT
.ERROR8  MNOTE 12,'BASE CAN BE ONLY 4 TO 12'
         MEND
*
         MACRO
&NAME    BEG   &ID,&MODE=REUS,&WKSIZE=0,&SP=0,&BASE=12,&RMODE=24
         GBLC  &RMD
&RMD     SETC  '&RMODE'
&NAME    CSECT
         AIF   ('&RMODE' EQ '24').NOT311
&NAME    RMODE ANY
&NAME    AMODE ANY
.NOT311  ENTRY &NAME
         USING *,&BASE
         ANF   &ID,MODE=&MODE,WKSIZE=&WKSIZE,SP=&SP,BASE=&BASE
         MEND
*
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
         REGISTER
USERID   BEG   *,MODE=RENT,RMODE=ANY
*
         L     R9,0(R1)           SAVE ADDRESS OF ARGUMENT
*
**********************************************************************
*                                                                    *
*  BRANCH TO AMODE 31 CODE TO GET THE USERID AND RETURN TO CALLER    *
*                                                                    *
**********************************************************************
*
         LA    R15,ABOVE           LOAD BRANCH ADDRESS
         L     R1,BIT32
         OR    R15,R1              SET AMODE 31 BIT
         BASSM R14,R15             BRANCH, SET AMODE AND SAVE OLD AMODE
         SCHL  RET=(R15),MODE=RENT
*
**********************************************************************
*                                                                    *
*  GET RACF USERID IN 31 BIT MODE                                    *
*                                                                    *
**********************************************************************
*
ABOVE    L     R2,CVTPTR
         USING CVTMAP,R2           MAKE CVT ADDESSABLE
*
*        RACF ACTIVE ?
*
         SR    R1,R1
         ICM   R1,7,CVTRAC+1       CHECK ADDRESS OF RCVT IN CVT
         BZ    RET12               IF ZERO, RACF IS NOT ACTIVE
         USING RCVT,R1             MAKE RCVT ADDRESSABLE
         TM    RCVTSTAT,RCVTRNA    IS RACF NOT ACTIVE ?
         BNZ   RET12               YES, RACF IS NOT ACTIVE
         DROP  R1
*
*        MAKE ACCESSOR ENVIREMENT ELEMENT (ACEE) AVAILABLE
*
         L     R2,CVTTCBP          ADDRESS OF TCBWORDS
         L     R2,12(R2)           ADDRESS OF CURRENT ASCB
         USING ASCB,R2
*
         CLC   ASCBASCB(4),=CL4'ASCB'  THIS AN ASCB REALLY ?
         BNE   RET8                NO, BRANCH
*
         L     R2,ASCBASXB         ADDRESS OF ADDRESS SPACE EXTENS.BL.
         USING ASXB,R2
         L     R2,ASXBSENV         ADDRESS OF ACCESSOR ENVIREM. ELEM.
         LTR   R2,R2               ADDRESS OF ACEE OK ?
         BZ    RET8                NO, BRANCH
         USING ACEE,R2
*
         CLC   ACEEACEE(4),=CL4'ACEE'  THIS AN ACEE REALLY ?
         BNE   RET8                NO, BRANCH
*
*        CHECK LENGTH
         XR    R1,R1
         ICM   R1,B'0001',ACEEUSRL GET USERID LENGHT
         LTR   R1,R1               IS IT POSITIVE ?
         BNP   RET4                NO, RET4
         CH    R1,MAXLEN           IS IT TOO LONG ?
         BH    RET4                YES, RET4
*
         CLI   ACEEUSRI,C'*'       USERID SUPPLIED AT RACINIT ?
         BE    RET4                NO, RET4
*
         MVC   0(8,R9),BLANK
         BCTR  R1,0
         EX    R1,MOVEUSR          USER ID TO CALLERS ARGUMENT
*
         XR    R15,R15
RET15    BSM   R0,R14              RETURN AND RESET TO OLD AMODE
*
RET4     LA    R15,4
         B     RET15
RET8     LA    R15,8
         B     RET15
RET12    LA    R15,8
         B     RET15
*
**********************************************************************
*                                                                    *
*  FIXED DATA AREA                                                   *
*                                                                    *
**********************************************************************
*
         DS    0F
BIT32    DC    XL4'80000000'
MOVEUSR  MVC   0(8,R9),ACEEUSRI
MAXLEN   DC    H'8'
BLANK    DC    CL8' '
         LTORG
*
**********************************************************************
*                                                                    *
*  D S E C T S                                                       *
*                                                                    *
**********************************************************************
*
         CVT   DSECT=YES,LIST=NO
         ICHPRCVT
         IHAACEE
         IHAASCB
         IHAASXB
         END
#endif
