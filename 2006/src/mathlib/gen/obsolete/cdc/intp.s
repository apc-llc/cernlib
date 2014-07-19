*
* $Id: intp.s,v 1.1.1.1 1996/04/01 15:03:01 mclareni Exp $
*
* $Log: intp.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:01  mclareni
* Mathlib gen
*
*
#if defined(CERNLIB_CDC)
          IDENT  INTP
          ENTRY INTP
* FUNCTION INTP(I1,I2)
 NAME     VFD    24/4HINTP,36/2
 INTP     BSS         1
          SB1    X1
          SA1    A1+1
          SB2    X1
          SX3         0
          SA1         B2               .J1=J
          ZR     X1,FINISH
          BX6         X1
          SA1         B1               .I1=I
          BX7         X1
          PL     X6,CONT
          BX6         -X6
          BX7         -X7
 CONT     SA6         J1
          SA7         I1
 CC       LX6         59               .INCREASE X3 WITH I1 IF J1 IS ODD
          PL     X6,AA
          SA2         I1
          IX3         X2+X3
 AA       SA2         J1               .IS J1 EQ 1
          SX4         1
          IX7         X2-X4
          NZ     X7,BB
 FINISH   BX6         X3               .STORE RESULT AND RETURN
          EQ          INTP
 BB       SA1         I1               .I1=2*I1
          LX1         1
          BX6         X1
          SA6         I1
          SA1         J1               .J1=J1/2
          AX1         1
          BX6         X1
          SA6         J1
          JP          CC
 I1       BSS         1
 J1       BSS         1
          END
#endif
