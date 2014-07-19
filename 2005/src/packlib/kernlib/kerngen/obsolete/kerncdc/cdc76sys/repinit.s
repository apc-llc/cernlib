*
* $Id: repinit.s,v 1.1.1.1 1996/02/15 17:51:30 mclareni Exp $
*
* $Log: repinit.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:30  mclareni
* Kernlib
*
*
          IDENT REPINIT
*
* CERN PROGLIB# Z038    REPINIT         .VERSION KERNCDC  0.1   751115
*
          ENTRY REPINIT,REPOK
          EXT   ABEND
*         30-NOV-73
*
          USE   /EXCHPAK/
 JCONDT   BSS   1
 JCODE    BSS   1
 LREP     BSS   1
 XCHC     BSS   16
 PARITY   BSS   2
          USE
*
*----     INITIALIZE   --  CALL REPINIT (REC-ADR,MASKPSD,MASKBPA)
*
          VFD   42/7HREPINIT,18/REPINIT
 REPINIT  DATA  0
          BX6   X1                 SET RECOVERY ADR
          SA6   LREP
          SA2   A1+1               CONSTRUCT RECOVERY WORD
          SA4   RWDEF
          ZR    X2,DEF             TAKE DEFAULT VALUE IF 2ND PAR. MISSIN
          SA3   A2+1
          SA2   X2
          SA3   X3
          MX0   30
          LX2   12
          BX2   X2+X3
          BX4   X0*X4
          BX2   -X0*X2
          BX4   X2+X4
 DEF      BX7   X4
          SA7   RWORD
          SA1   REPINIT
          BX6   X1
          SA6   REPOK
          EQ    REPOK+1
*
*         EXCHANGE JUMP PACKAGE RECIEVED
 FWA      BSSZ  17
*
*------   RECOVERY PROCEDURE STARTS HERE, FIRST COPY XJPK TO COMMON
          SA1   FWA                COPY FIRST WORD
          SB7   1              B7= 1
          SB1   16                 COPY 16 MORE WORDS
          BX7   X1
          SA7   XCHC
          BX5   X1             X5= BPA
 LOOP1    SA1   A1+B7
          SB1   B1-B7
          BX7   X1
          SA7   A7+B7
          NE    B1,B0,LOOP1
*
*------   SET RJ TO RECOVERY ROUTINE
          SA3   LREP
          SA4   RJINST
          LX3   30
          BX6   X3+X4
          SA6   RJ
*
*------   CONVERT PSD OR BPA TO ERROR CONDITION AND CODE
          SB1   X5             B1= BPA
          NE    B1,B0,EXTER
*
*---      HANDLE PSD CONDITION
          SA1   FWA+3
          MX5   12
          LX1   12
          BX1   X5*X1
          LX1   12             X1= 12 CONDITION FLAGS, RIGHT JUST.
          NX7   B1,X1
          SB2   48
          SX7   B2-B1          X7= ERR CONDITION NUMBER
          EQ    JOIN
*
*---      EXTERNAL CONDITION BPA
 EXTER    MX7   0              X7= 1, 2, 3, ...
          SX5   B7             X5= 1
          SB5   6              B5= LOOP COUNT
          SA3   LIST           X3= CODE POSSIBILITIES
          MX0   54             X0= MASK ...77700
*
 LOOPEX   LX3   6
          BX2   -X0*X3         X2= CODE POSS.
          IX7   X7+X5          X7= ERR CONDITION NO
          SB2   X2
          EQ    B2,B1,ENDLEX       BPA = CODE
          SB5   B5-B7
          NE    B5,B0,LOOPEX
*
*         CHECK SECOND OCCCURRENCE OF  BPA CONDITION
 ENDLEX   SA3   OCCURD         X3= PREVIOUS OCC.
          SB5   X7             B5= 1, 2, 3, ...
          MX2   1
          LX2   B5,X2          X2= MASK WITH BIT 1, 2, 3, ...
          BX6   X2*X3
          NZ    X6,EXABEND         EXIT FOR SECOND OCC.
          BX6   X2+X3          X6= OCCURD UPDATED
          SA6   A3
          SX7   B5+12          X7= 13, 14, 15
*
 JOIN     SA5   X7+WORDS-1         CODE-WORD
          SX7   X7+900             JCONDT= 900 + 1, 2, 3, ...
          BX6   X5
          SA7   JCONDT
          SA6   A7+B7
          SA1   PARAM
 RJ       DATA  0                  RETURN-JUMP TO RECOVERY ROUTINE
 EXABEND  RJ    ABEND
*
*----     RESET RECOVERY  CALL REPOK
*
 REPOK    DATA  0
          SA1   RWORD
          BX6   X1
          SA6   FWA
          REPRIEVE FWA
          MX7   0
          SA7   JCODE
 LWA      EQ    REPOK
*
 RWDEF    VFD   30/LWA,30/77470004B
 RWORD    DATA  0
 RJINST   VFD   6/1,54/REPINIT-1
 LIST     VFD   30/0610011721B,30/0
 OCCURD   DATA  0
 PARAM    DATA  0
 WORDS    VFD  60/10HUNDERFLOW   901    *BL
          VFD  60/10HOVERFLOW    902    *BL
          VFD  60/10HINDEFINITE  903    *BL
          VFD  60/10HSTEP        904    *BL
          VFD  60/10HBREAKPOINT  905    *BL
          VFD  60/10HPROG RANGE  906    *BL
          VFD  60/10HSCM RANGE   907    *BL
          VFD  60/10HLCM RANGE   908    *BL
          VFD  60/10HSCM BRANGE  909    *BL
          VFD  60/10HLCM BRANGE  910    *BL
          VFD  60/10HSCM PARITY  911    *BL
          VFD  60/10HLCM PARITY  912    *BL
          VFD  60/10HOP DROP     913    *BL
          VFD  60/10HRERUN       914    *BL
          VFD  60/10HTIME LIMIT  915    *BL
          VFD  60/10HMASS LIMIT  916    *BL
          VFD  60/10HLCM LIMIT   917    *BL
          VFD  60/10H????        918    *BL
          END
