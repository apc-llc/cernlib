*
* $Id: iuback.s,v 1.1.1.1 1996/02/15 17:51:21 mclareni Exp $
*
* $Log: iuback.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:21  mclareni
* Kernlib
*
*
          IDENT IUBACK
*
* CERN PROGLIB# M410    IUBACK          .VERSION KERNCDC  1.06  780614
*
          ENTRY IUBACK
*
*     NVAL = IUBACK (CH,JL,JR)
*                              COMPASS VERSION  MAY 1978, JZ
*
*         REGISTER ASSIGNMENTS
*
*         X0  MASK  7700-00
*         X1  CH(J)            B1  JN = J-1
*         X2  CH(J-1) AHEAD    B2  JL
*         X3  FACTOR OF 10     B3  = -27 = DC FOR ZERO (DISPLAY CODE)
*         X4  = 10             B4          DC FOR CH(J)  - 27
*         X5  NVAL CUMULATION  B5  = 18  = DC FOR BLANK  - 27
*         X6  DIGITS READ      B6  = 10  = DC FOR PLUS   - 27
*                              B7  = 1
*
          USE   /SLATE/
 NDIGIT   BSS   1
 JEND     BSS   39
          USE
*
          VFD   36/6HIUBACK,24/IUBACK
 IUBACK   BSSZ  1              X1= ADR(CH)
          SB7   1
          SA2   A1+B7          X2= ADR(JL)
          SA3   A2+B7          X3= ADR(JR)
          MX0   6
          SB3   -27
          MX5   0
          SA2   X2             X2= JL
          SA3   X3             X3= JR
          SB5   18
          SB6   10
          MX6   0
          SX4   B6             X4= 10
          SB1   X3             B1= JN = JR
          SB2   X2             B2= JL
          SB4   B1-B7
          SA2   X1+B4          X2= CH(JN) = CH(JR)
          SX3   B7             X3= FACTOR = 1 INIT
          LT    B1,B2,BLANK
*
 LOOP     BX1   X0*X2          X1= CH(J)
          SA2   A2-B7          X2= CH(JN)  READ AHEAD
          SB1   B1-B7          B1= JN = J-1
          LX1   6
          SB4   X1+B3          B4= DC - 27
          LT    B4,B0,END          -->  DC .LT. 27
          EQ    B4,B5,BLANK        -->  DC .EQ. 45
          GE    B4,B6,END          -->  DC .GE. 37
          SX1   B4
          DX1   X1*X3          X1= VAL*FACTOR
          SX6   X6+B7
          IX5   X1+X5          X5= SUM = SUM + VAL*FACT
          DX3   X3*X4          X3= FACT = FACT * 10
 BLANK    GE    B1,B2,LOOP
          SB1   B1-B7              JN = JN - 1
 END      SA6   NDIGIT             STORE NUMBER OF DIGITS READ
          SX7   B1+B7              JEND = J = JN+1
          SA7   A6+B7              STORE JEND
          BX6   X5
          EQ    IUBACK
          END
#ifdef CERNLIB_TCGEN_IUBACK
#undef CERNLIB_TCGEN_IUBACK
#endif
