*
* $Id: xtpch2.s,v 1.1.1.1 1996/02/15 17:51:18 mclareni Exp $
*
* $Log: xtpch2.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:18  mclareni
* Kernlib
*
*
          IDENT       CLVLZL
*
* CERN PROGLIB# Z200    XTPCH2          .VERSION KERNCDC  1.16  820129
*
          ENTRY       CLVLZL
          ENTRY       XTPCH2
* SUBROUTINE XTPCH2(LUN,NERROR)
*     SUBROUTINE CLVLZL (LUN,NERROR)
 TRACE.   VFD     42/0HCLVLZL,18/CLVLZL
*
 TEMPA0   BSS         1
 RC       BSSZ   1           TO SAVE RC
 ADRC     BSSZ   1           TO SAVE RC ADDRESS
 MSG      DATA   H*CLVLZL - RM ERROR = *
 RMNR     DATA   H*      ON *
 LFN      BSSZ   1
*
 CLVLZL   DATA        0
 XTPCH2   EQU         CLVLZL
          SX6         A0
          MX7    0
          SA2    A1+1        X2=ADDRESS OF NERROR
          SA6         TEMPA0
          SA7    RC          PRESET RC TO ZERO
          BX6    X2
          NO
          SA6    ADRC        SAVE ADDRESS OF NERROR
          MX6   1
          BX1   X1+X6
 +        RJ     =XGETFIT.
 -        VFD    30/TRACE.
          STORE       X1,EX=RECOV
          CLOSEM      X1,R,VOLUME
 END      BSS    0
          SA2    ADRC
          SA5         TEMPA0
          SA0         X5               .RESTORE A0
          SA3    RC
          BX6    X3
          SA6    X2          RETURN RC TO USER
          EQ          CLVLZL
*
 RECOV    BSS    1           ENTRY FOR RM ERROR
          FETCH  X1,IRS,X6
          SA6    RC          SAVE FIES
          BX3    X6
*
*         TRANSFORM INTEGER IN DISPLAY CODE
          MX6    0
          MX0    60-3
          SB5    3           ONLY 3 DIGITS IN RM ERROR
          LX3    51
          SB1    1
          SB4    1R0
 COD1     BSS    0
          LX3    3
          BX4    -X0*X3
          SX5    X4+B4
          LX6    6
          IX6    X6+X5
          SB5    B5-B1
          NZ     B5,COD1
          LX6    42
          MX0    18
          SA5    RMNR
          BX4    -X0*X5
          BX6    X0*X6
          IX6    X4+X6
          SA6    A5          STORE RMNR
          SA2    X1
          BX6    X2
          SA6    LFN         SAVE LFN NAME
          MESSAGE MSG
          STORE  X1,ES=0     CLEAR FIES
          EQ     END
          END
