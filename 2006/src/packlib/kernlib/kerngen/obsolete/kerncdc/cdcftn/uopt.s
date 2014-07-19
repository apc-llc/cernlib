*
* $Id: uopt.s,v 1.1.1.1 1996/02/15 17:51:26 mclareni Exp $
*
* $Log: uopt.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:26  mclareni
* Kernlib
*
*
          IDENT UOPT
*
* CERN PROGLIB# M502    UOPT            .VERSION KERNCDC  0.1   750820
*
*         SUBROUTINE UOPT(DATA,POSS,IOPT,NP)
*
*         PROGRAM P.RASTL, JULY 1971
*
          ENTRY UOPT
          VFD   24/4HUOPT,36/UOPT
 UOPT     BSS   1
          SB7   -1
          SA2   A1-B7
          BX0   X0-X0              ALL FLAGS OFF
          SB2   X2
          SA1   X1             X1= FIRST WORD OF DATA
          MX3   6              X3= MASK
          SA4   A2-B7
          SX5   B0-B7
          SB3   X4
          SA2   A4-B7
          SA4   X2             X4= NP
          SB5   X4-10
          GE    B0,B5,SPEC         SPECIAL VERSION, IF NP.LE.10
          SA2   B2+B7          A2= B2-1
          SB4   X4
          SB6   6
          BX2   X2-X2
 FIND     SB4   B4+B7
          NZ    X2,CONT            READ NEW POSSIBILITIES
          SA2   A2-B7
 CONT     BX6   X1-X2              COMPARE CHARACTER
          LT    B4,B0,END          CHARACTER NOT FOUND
          BX6   X3*X6
          BX7   -X3*X2
          LX2   B6,X7
          NZ    X6,FIND
          BX7   -X3*X1             CHARACTER EQUAL
          LX6   B4,X5
          SA2   B2
          SB4   X4
          BX0   X0+X6              SET FLAG
          LX1   B6,X7
          NZ    X7,FIND            READ NEW DATA
          SA1   A1-B7
          EQ    FIND
 SPEC     SA2   B2                 SPECIAL VERSION
          LX1   54
 NEXT     SB4   X4
          BX7   X2
          LX1   6
 FIND1    SB4   B4+B7
          BX6   X1-X7              COMPARE CHARACTER
          LT    B4,B0,END          CHARACTER NOT FOUND
          BX6   X3*X6
          LX7   6
          NZ    X6,FIND1
          LX6   B4,X5              CHARACTER EQUAL
          BX0   X0+X6              SET FLAG
          EQ    NEXT
 END      BX6   X5*X0              STORE FLAGS
          SX4   X4+B7
          AX0   1
          SA6   X4+B3
          NZ    X4,END
          EQ    UOPT
          END
#ifdef CERNLIB_TCGEN_UOPT
#undef CERNLIB_TCGEN_UOPT
#endif
