*
* $Id: trac1q.s,v 1.1.1.1 1996/02/15 17:51:38 mclareni Exp $
*
* $Log: trac1q.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:38  mclareni
* Kernlib
*
*
          IDENT TRAC1Q
*
* CERN PROGLIB# N105    TRAC1Q          .VERSION KERNCDC  1.13  820111
*
          ENTRY TRAC1Q,TRAC2Q
*         30-NOV-73
*
*         M-LAN ROUTINE FTN CDC 6/7000  FOR TRACEQ
*
          USE   /SLATE/
 NAME     BSS   1
 LINE     BSS   1
 LADR     BSS   1
 LOK      BSS   1
 LWK      BSS   36
*
          USE
 TRAC1Q   DATA  0
          SA1   TRAC1Q         X1= ENTRY-EXIT LINE OF TRAC1Q
          SB7   -1
          SX6   B7
          SA6   LADR
          LX1   30
          SA2   X1+B7          X2= RJ TO TRACE1Q
          SA3   X2             X3= TRACE-WORD OF TRACEQ
          BX6   X3
          SA6   NAME               STORE TRACE-WORD OF TRACEQ
*
          MX7   0
          SA7   LWK
          MEMORY CM,LWK,RECALL     GET FIELD LENGTH
          SA1   LWK            X1= FIELD LENGTH IN LEFT HALF
          AX1   30
          BX7   X1
          SA7   A1                 LWK= FIELD-LENGTH RIGHT JUSTIFIED
          EQ    TRAC1Q
*
 TRAC2Q   DATA  0
          SA1   NAME           X1= DOWN TRACE-WORD
          SA5   LWK
          SB7   -1             B7= -1
          SB2   X1             B2= ADR OF DOWN ENTRY-EXIT LINE
          SB5   X5             B5= FIELD-LENGTH
          LT    B2,B0,FAULT        B2 OUT OF RANGE
          GE    B2,B5,FAULT
          SA2   B2             X2= ENTRY-EXIT LINE DOWN
          BX6   X2
          AX6   54                 CHECK OP-CODE 04
          SB1   X6-4
          NE    B1,B0,MAIN
          LX2   30
          SB1   X2+B7          B1= ADR OF RJ TO DOWN
          LT    B1,B0,FAULT        B1 OUT OF RANGE
          GE    B1,B5,FAULT
          SX6   B1
          SA6   LADR
          SA1   B1             X1= RJ-WORD TO DOWN
          BX4   X1                 CHECK OP-CODE  0100B
          AX4   48
          SB4   X4-100B
          NE    B4,B0,FAULT
          SB3   X1             B3= ADR OF UP TRACE-WORD
          LX1   30
          MX4   12
          BX6   X1*X4
          LX6   12
          SA6   LINE               STORE LINE NUMBER
          LT    B3,B0,FAULT        B3 OUT OF RANGE
          GE    B3,B5,FAULT
          SA4   B3             X4= UP TRACE-WORD
          BX7   X4
          SA7   A6+B7              STORE UP TRACE-WORD INTO NAME
          SX6   B1
          SA6   A6-B7              STORE ADR OF RJ-WORD INTO LADR
          EQ    TRAC2Q
 MAIN     SX6   B0-B7
          SA6   LOK                LOK = +1
          EQ    TRAC2Q
 FAULT    SX6   B7
          SA6   LOK                LOK = -1
          EQ    TRAC2Q
          END
