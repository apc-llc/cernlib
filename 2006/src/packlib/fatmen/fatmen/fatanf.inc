*
* $Id: fatanf.inc,v 1.1.1.1 1996/03/07 15:17:59 mclareni Exp $
*
* $Log: fatanf.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:59  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATANF_INC
#define CERNLIB_FATMEN_FATANF_INC
*
*
* fatanf.inc
*
#if defined(CERNLIB_IBMMVS)
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
#endif

#endif
