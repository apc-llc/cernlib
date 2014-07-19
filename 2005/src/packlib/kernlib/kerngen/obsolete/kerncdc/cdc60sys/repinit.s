*
* $Id: repinit.s,v 1.1.1.1 1996/02/15 17:51:32 mclareni Exp $
*
* $Log: repinit.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:32  mclareni
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
          SX4   25B                DEFAULT RECOVERY WORD
          ZR    X2,DEF             TAKE DEFAULT IF 2ND PAR. MISSING
          SA3   A2+1
          SA2   X2             X2= JPSD
          SA3   X3             X3= JBPA
          SX6   1706B
          SX7   4000B
          SX4   14B
          BX6   X6*X2
          BX7   X7*X2
          BX4   X4*X3          X4= BIT   3 TIME, 4 DROP
          ZR    X6,NOAR
          SX4   X4+1           X4= BIT   1 ARITH. CONDITIONS
 NOAR     ZR    X7,DEF
          SX4   X4+20B         X4= BIT   5 ECS PARITY
 DEF      BX7   X4
          SA7   RWORD
          SA1   REPINIT
          BX6   X1
          SA6   REPOK
          EQ    REPOK+1
*
*------   RECOVERY PROCEDURE STARTS HERE, FIRST COPY XJPK TO COMMON
 GO       DATA  0              X1= ADR(EXCHPK)
          SA1   X1
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
          SB2   2
          NE    B1,B2,EXTER
*
*---      HANDLE PSD CONDITION
          SA1   B0             X1= WORD 0 (RA) = 000/C/P-COUNTER
          MX0   18
          LX0   54             X0= 00777777000000000000
          LX1   6              X1= WORD 0 ROTATED BY 6 PLACES
          BX6   X0*X1              P-COUNTER PICKED
          BX7   -X0*X5             P-COUNTER CLEARED IN EXCH-P
          BX6   X6+X7
          SA6   XCHC               P-COUNTER STORED INTO EXCH-P
*
          SA2   PSDCV          X2= 07236
          MX0   3              X0= 70000...
          BX1   X0+X1          X1= 7/C/P-COUNTER
          LX1   6
 LOOPSD   LX1   59
          LX2   3
          PL    X1,LOOPSD
          BX7   X0*X2
          LX7   3              X7= 2 OVFL, 3 INDEF, 6 PROG RG, 7 SCM RG
          EQ    JOIN
*
*---      EXTERNAL CONDITION BPA
 EXTER    SA3   LIST           X3= 01 06 07 10 12 17 21 77 77 77
          SA4   BPACV          X4= 04 02 02 03 01 05 04 07 07 07
          SB5   9              B5= LOOP COUNT
          MX0   54             X0= MASK  ...7777700
*
 LOOPEX   LX3   6
          LX4   6
          BX2   -X0*X3
          BX7   -X3*X4         X7= ERR NO - 911
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
          SX7   B5+11          X7= 12, 13, 14, 15, 16, 17, 18 = EC-900
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
          MX7   0
          SA7   B0                 CLEAR RA TO ZERO
          SA7   JCODE              CLEAR JCODE TO ZERO
          SA2   RWORD
          RECOVR GO,X2,LWA
 LWA      EQ    REPOK
*
 RWORD    DATA  0
 RJINST   VFD   6/1,54/REPINIT-1
 PSDCV    VFD   15/07236B,45/0
 BPACV    DATA  04020203010504070707B
 LIST     DATA  01060710121721777777B
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
