*
* $Id: trac0q.s,v 1.1.1.1 1996/02/15 17:51:38 mclareni Exp $
*
* $Log: trac0q.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:38  mclareni
* Kernlib
*
*
          IDENT TRAC0Q
*
* CERN PROGLIB# N105    TRAC0Q          .VERSION KERNCDC  1.03  771219
*
          ENTRY TRAC0Q
*         30-NOV-73
*
          USE   /EXCHPAK/
 JCONDT   BSS   1
 JCODE    BSS   1
 LREP     BSS   1
 XCHC     BSS   16
 PARITY   BSS   2
*
          USE   /SLATE/
 NAME     BSS   1
 LINE     BSS   1
 LADR     BSS   1
 LOK      BSS   1
 LWK      BSS   36
*
          USE
 TRAC0Q   DATA  0
          SB7   -1             B7= -1
          SX6   B7
          SA6   LOK                LOK-FLAG= -1  (NO TRACE FOUND)
          SA6   A6+B7              LADR = -1  IF EXCHJPK NOT SET
          SA1   JCODE              CHECK IF SET
          ZR    X1,TRAC0Q
          SB6   X1
          SB6   A1-B6
          EQ    B6,B0,TRAC0Q       EXIT FOR JCODE PRESET INDEF
*
*         GET FIELD LENGTH
          MX7   0
          SA7   LWK
          MEMORY CM,LWK,RECALL
          SA1   LWK
          SB7   -1             B7= -1
          LX1   30
          SB6   X1             B6= RFL
          SX6   B6
          SA6   LWK                LWK= RFL FOR TRAC2Q
*
*         GET P-COUNTER
          SA1   XCHC
          LX1   24
          SB4   X1             B4= P
          SX6   B4
          SA6   LADR               LADR= P
          SB5   100B           B5= 100B  (PROGRAM START)
          LT    B4,B5,TRAC0Q       EXIT FOR PROGRAM RANGE
          GE    B4,B6,TRAC0Q
*
*         SEARCH MATCHING  RETURN-JUMP/HOME-JUMP  (RJ/HJ)
          SB1   4100
          SX6   B4-B1
          PL    X6,FULL
          SB1   B4
 FULL     SB1   B1-B5          B1= MIN0(4000,P-100B)
          SB1   B1-B7
          MX4   42
          LX4   30             X4= MASK  12/7777,18/0,30/777...
          MX5   1
          LX5   57             X5= MASK  6/04,54/0  (EQ 0/ 0)
          MX0   30             X0= MASK  30/777..., 30/0
          SA1   B4-B7          A1= P+1
*
*         SEARCH LOOP
 LOOPS    SA1   A1+B7          X1=  EQ HOME-ADR / 0  SUPPOSED
          SB1   B1+B7
          EQ    B1,B0,TRAC0Q       EXIT FOR RANGE EXHAUSTED
          BX6   X1*X4
          BX6   X6-X5              COMPARE TO  EQ 0 / 0
          NZ    X6,LOOPS
          BX6   X1
          LX6   30
          SB2   X6+B7          B2= ADR OF RJ = HOME-ADR - 1
          LT    B2,B5,LOOPS        ADR OF RJ OUT OF RANGE
          GE    B2,B6,LOOPS
          SA2   B2             X2= RJ ENTRY-ADR/ --  SUPPOSED
          BX6   X2*X0
          LX6   30                 0 / RJ ENTRY-ADR
          SB4   A1             B4= ENTRY-ADR
          SB3   X6             B3= ADR IN RJ
          NE    B3,B4,LOOPS        ADR MIS-MATCH
          AX6   18
          SB3   X6-100B
          NE    B3,B0,LOOPS        FUNCTION CODE NOT  RJ
*
*         A MATCH HAS BEEN FOUND, LOOK WHETHER THERE IS A TRACE STARTING
          SB3   X2             B3= ADR OF UPPER TRACE-WORD SUPPOSED
          LT    B3,B5,LOOPS        CHECK B3 IN RANGE
          GE    B3,B6,LOOPS
*
*         LOOKS FINE, NOW FIND THE TRACE-WORD IN LOWER ROUTINE
          SX6   B4
          SA6   LINE               LINE= SUPPOSED ENTRY ADR
          SB2   B1
          SA2   A1
 LOOPT    SA2   A2+B7          X2= SUPPOSED TRACE-WORD
          SB2   B2+B7
          EQ    B2,B0,LOOPS        NO TRACE-WORD FOUND: TRY DIFFERENT
          SB3   X2             B3= ADR IN TRACE-WORD
          EQ    B3,B4,IFNAME       ADRS MATCH, CHECK NAME LEGAL
          BX3   X2-X1              CHECK SAME ENTRY/EXIT LINE
          NZ    X3,LOOPT
          SB4   A2                 UPDATE ENTRY-ADR
          EQ    LOOPT
*
*         CHECK ROUTINE NAME LEGAL (FORTRAN CHAR. SET)
 IFNAME   MX7   42
          BX2   X7*X2          X2= NNNNNNN 000
          MX7   54
          SX6   48             X6= FORTRAN SET LIMIT
 LOOPN    LX2   6
          BX3   -X7*X2         X3= SINGLE CHARACTER
          BX2   X7*X2          X2= REMAINING CHARACTERS
          ZR    X3,LOOPT           CHAR. ZERO
          IX3   X6-X3          X3= 48-CHAR.VALUE
          NG    X3,LOOPT           NOT FORTRAN CHAR.
          NZ    X2,LOOPN
*
*         ALL LOOKS WELL, EVERYTHING FOUND
          MX7   0
          SA7   LOK                LOK=0
          SX6   A2                 ADR(TRACE-WORD)
          SA6   LWK+1
          SA2   X6                 TRACE-WORD
          BX6   X2
          SA6   NAME
          EQ    TRAC0Q
          END
