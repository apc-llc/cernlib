*
* $Id: dispza.s,v 1.1.1.1 1996/04/01 15:03:18 mclareni Exp $
*
* $Log: dispza.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:18  mclareni
* Mathlib gen
*
*
          IDENT  DISPZA
          ENTRY  DISPZA
* SUBROUTINE DISPZA(FILENAME,DISPOSITION,STATION)
*   MODIFIED BY T LINDELOF, AUGUST 1978
 DISPZA   BSS    1
          SA2    X1
          MX6    6
          BX3    X6*X2
          ZR     X3,ARGOK
          BX3    X2
          SX5    55B
          LX5    54
          MX6    6
          SB5    47
          SB1    -1
 REC      BX4    X3*X6
          BX4    X4-X5
          ZR     X4,TERM
          SB1    B1+6
          LX3    6
          EQ     B1,B5,ERR2
          EQ     REC
 TERM     MX3    1
          AX3    B1,X3
          BX2    X2*X3
 ARGOK    BX7    X2
          SA7    ARG
          USE    BBB
 ARG      BSS    1
 SAVEA0   BSS    1
 SAVEA1   BSS    1
          USE    *
          SX7    A0
          SX6    A1
          SA7    SAVEA0
          SA6    SAVEA1
          SX1    ARG
          MX6     1
          BX1     X1+X6
 +        RJ     =XGETFIT.   GET FIT ADDRESS IN X1
          BX3    X1
          SA4    SAVEA0
          SA0    X4
          SA4    SAVEA1
          SA1    X4
          USE    AAA
 FICH     VFD      60/0
 FICH1    VFD      60/0
          VFD      60/0
          USE    *
          SA4      X3
          MX0    42
          BX7    X0*X4
          SA7      FICH
          SA4      A1+1
          SA2      X4
          AX2      48
          MX6      48
          BX2      -X6*X2
          SB1      X2
          SX2      40B             PRINT?
          SB2      2022B           PR
          EQ       B1,B2,ARG3
          SX2      10B             PUNCH?
          SB2      2025B           PU
          EQ       B1,B2,ARG3
          SB2      2044B           P9
          EQ       B1,B2,ARG3
#if !defined(CERNLIB_NOSBE)
          SX2      67B             DROP?
          SB2      2303B           SC
          EQ       B1,B2,ARG3
#endif
          EQ       ERR1
 ARG3     SA5      A1+2
          ZR       X5,PARG
          SA4      X5
          SX7      55B
          LX7      36
          SX6      77B
          LX6      36
          BX6      X6*X4
          BX7      X7-X6
          ZR       X7,ARGP
          MX7      36
          EQ       CHAR
 ARGP     MX7      18
 CHAR     BX4      X7*X4
 CHAR1    BX2      X4+X2
          SX4      300B
          BX7      X4+X2
          SA7      FICH1
+MJ105B
-         VFD    30/FICH
          OPENM X3,OUTPUT,N
          EQ     DISPZA
 ERR1     MESSAGE MESS1,,
          EQ     DISPZA
 ERR2     MESSAGE MESS2,,
          EQ     DISPZA
 PARG     SA4    DEF
          EQ     CHAR1
 DEF      VFD    18/030320B,42/0
#if defined(CERNLIB_NOSBE)
 MESS1    DIS    ,*DISPZA-DISPOSITION IS NOT PR OR PU*
#endif
#if !defined(CERNLIB_NOSBE)
 MESS1    DIS    ,*DISPZA-DISPOSITION IS NOT PR,PU OR SC*
#endif
 MESS2    DIS    ,*DISPZA-FILENAME TOO LONG*
          END
