*
* $Id: ubunch.s,v 1.1.1.1 1996/02/15 17:51:25 mclareni Exp $
*
* $Log: ubunch.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:25  mclareni
* Kernlib
*
*
          IDENT UBUNCH
*
* CERN PROGLIB# M409    UBUNCH          .VERSION KERNCDC  2.07  831121
*
          ENTRY UBUNCH
#if !defined(CERNLIB_F77)
          ENTRY UH1TOC
#endif
*
*     SUBROUTINE UBUNCH (KBLOWN,KBUNCH,N)
*        (WRITTEN FOR SCOPE 3.3 - FTN)
          VFD   36/6HUBUNCH,24/UBUNCH
 UH1TOC   BSS   0
 UBUNCH   BSS   1
          SA5   A1+2
          SA2   A1+1           X2= ADDRESS OF KBLOWN
          SA3   X5             X3= N
          MX7   6                  MASK
          SB7   -6
          SA4   X1             X4= KBLOWN
          MX0   59             X0= -1
          ZR    X3,UBUNCH
 LOOP1    MX6   0
          SB6   X0             B6= -1
          SB4   60                 CHARACTER-COUNT
 LOOP2    LX4   B4,X4
          SB4   B4+B7
          BX5   X7*X4
          IX3   X3+X0
          LX7   54
          SA4   A4-B6
          NO
          BX6   X5+X6              THE RESULT
          ZR    X3,FINN
          NZ    B4,LOOP2
 FA       SA6   X2
          IX2   X2-X0
          NZ    B6,LOOP1
          EQ    UBUNCH
 FINN     SB6   B0
          ZR    B4,FA
          SA4   BLANKS
          EQ    LOOP2
 BLANKS   DIS   ,* *                    *BL
          END
#ifdef CERNLIB_TCGEN_UBUNCH
#undef CERNLIB_TCGEN_UBUNCH
#endif
#ifdef CERNLIB_TCGEN_UH1TOC
#undef CERNLIB_TCGEN_UH1TOC
#endif
