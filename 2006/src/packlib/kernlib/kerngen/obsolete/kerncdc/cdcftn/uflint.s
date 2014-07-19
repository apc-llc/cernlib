*
* $Id: uflint.s,v 1.1.1.1 1996/02/15 17:51:25 mclareni Exp $
*
* $Log: uflint.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:25  mclareni
* Kernlib
*
*
          IDENT UFLINT
*
* CERN PROGLIB# M251    UFLINT          .VERSION KERNCDC  1.02  771015
*
          ENTRY UFLINT
*
*     CALL UFLINT (VECT,NW,MODE)
*
*         PROGRAM WRITTEN 12-AUG-77, JZ
*
          VFD   36/6HUFLINT,24/UFLINT
 UFLINT   BSSZ  1
          SB7   1              B7= 1
          SA2   A1+B7          X2= ADR(NW)
          SA3   A2+B7          X3= ADR(MODE)
          SA1   X1             A1= ADR(VECT(J))
          SA2   X2             X2= NW
          SA3   X3             X3= MODE
          BX4   -X2
          SB6   X4             B6= /NW/
          NG    X2,NEXT
          LX3   59                 SET REPRESENTATION SELECTOR FOR NW +VE
          AX3   59
          SB6   X2             B6= NW
*
 NEXT     EQ    B6,B0,UFLINT       LOOP EXIT
          BX2   X1             X2= VECT(J)
          SA1   A1+B7          X1= VECT(J+1)
          SB6   B6-B7              BUMP LOOP COUNT
          LX3   59             X3= REPRES. SELECTOR IN SIGN-BIT
          BX4   X2
          AX4   48
          ZR    X4,INTG        --> VECT(J) HAS ZERO EXPONENT
          PL    X3,NEXT
*
*               CONVERT TO INTEGER
          ID    X2,NEXT
          OR    X2,NEXT
          UX0   B2,X2
          LX7   B2,X0
          SA7   A1-B7
          JP    NEXT
*
*               CONVERT TO FLOATING
 INTG     NG    X3,NEXT
          PX0   B0,X2
          NX7   B0,X0
          SA7   A1-B7
          JP    NEXT
          END
#ifdef CERNLIB_TCGEN_UFLINT
#undef CERNLIB_TCGEN_UFLINT
#endif
