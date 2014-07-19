*
* $Id: ublow.s,v 1.1.1.1 1996/02/15 17:51:25 mclareni Exp $
*
* $Log: ublow.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:25  mclareni
* Kernlib
*
*
          IDENT UBLOW
*
* CERN PROGLIB# M409    UBLOW           .VERSION KERNCDC  2.07  831121
*
          ENTRY UBLOW
#if !defined(CERNLIB_F77)
          ENTRY UCTOH1
#endif
*
*     SUBROUTINE UBLOW (KBUNCH,KBLOWN,N)
*        (WRITTEN FOR SCOPE 3.3 -- FTN)
          VFD   30/5HUBLOW,30/UBLOW
 UCTOH1   BSS   0
 UBLOW    BSS   1
          SB1   1              B1= 1
          SA4   BLANK              9 BLANKS PRECEEDED BY 00
          SA2   A1+B1          X2= ADDRESS OF KBLOWN
          MX0   6                  MASK FOR CHARACTERS
          SA3   A2+B1
          SA5   X1             A5= ADDRESS OF KBUNCH
          SB7   10
          SA1   X3
          MX3   59             X3= -1
          SB3   X1
          ZR    X1,UBLOW
 LOOP1    LX1   B0,X5
          SA5   A5+B1
          SB2   B7                 SET LOOP COUNT
          BX7   X1*X0              FETCH FIRST CHARACTER
 LOOP2    LX1   6                  PREPARE NEXT CHARACTER
          BX6   X7+X4              ADD BLANKS
          ZR    B2,LOOP1           ALL TEN DONE
          SB3   B3-B1
          SA6   X2                 STORE AWAY
          BX7   X1*X0              FETCH NEXT CHARACTER
          IX2   X2-X3              NEW STORE ADDRESS
          SB2   B2-B1              CHARACTER COUNT
          GE    B3,B1,LOOP2
          EQ    UBLOW              RETURN
 BLANK    DATA  00555555555555555555B
          END
#ifdef CERNLIB_TCGEN_UBLOW
#undef CERNLIB_TCGEN_UBLOW
#endif
#ifdef CERNLIB_TCGEN_UCTOH1
#undef CERNLIB_TCGEN_UCTOH1
#endif
