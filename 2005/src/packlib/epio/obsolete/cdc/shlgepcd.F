*
* $Id: shlgepcd.F,v 1.1.1.1 1996/03/08 15:21:52 mclareni Exp $
*
* $Log: shlgepcd.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:52  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CDC)
          IDENT  SHLGEP
          ENTRY  SHLGEP
*      SUBROUTINE SHLGEP(IA,NW,NBIT)
*      PERFORMS A LONG LEFT SHIFT (NBIT GT 0) OR RIGHT SHIFT
*      ( NB LT 0 ) CIRCULAR ON IA(1)...IA(NW)
*
 TRACE    VFD    42/0LSHLGEP,18/SHLGEP
 SHA0     DATA   0
 SHLGEP   JP     400000B+*
          SX6    A0
          SA0    A1
          SA6    SHA0              SAVE A0
          SB1    1
          SA2    A0+1              ADDRESS OF NW TO X2
          SB2    59
          SA3    A0+2              ADDRESS OF NBIT TO X3
          SA2    X2                NW TO X2
          SA3    X3                NBIT TO X3
          SB3    X2-1              NW-1 TO B3
          LT     B3,B0,RETURN      RETURN IF NW LE 0
          SB4    X3                NBIT TO B4
          SB5    B4
          GE     B4,B0,CONT1       JUMP IF B4 GE 0
          SB5    B0-B4             IABS(NBIT) TO B5
 CONT1    EQ     B5,B0,RETURN      RETURN IF NBIT EQ 0
          GT     B5,B2,RETURN          RETURN IF NBIT GE 60
          GE     B4,B0,POSIT       JUMP IF NBIT GT 0
*  RIGHT CIRCULAR SHIFT
          SB4    B4+B2             NO. OF MASK BITS -1
          MX7    1
          AX0    B4,X7             MASK1 TO X0
          SB4    B4+B1             SHIFT COUNT IN BITS
          SA1    A0                ADDRESS OF IA(1) TO X1
          SA4    X1+B3             KEEP LAST WORD IN X4
          SB6    X1                ADDRESS IA(1)
          SB7    A4                ADDRESS IA(NW)
 NLOOP    LT     B7,B6,RETURN      END LOOP
          SA2    B7                ADDR.(IA(I2)) TO A2, IA(I2) TO X2
          SB7    B7-B1             ADDR.(IA(I1))
          BX3    X4                LAST WORD IF AT END OF LOOP
          LT     B7,B6,NLOOP1
          SA3    B7                REPLACE LAST WORD BY IA(I1) IN X3
 NLOOP1   BX5    X0*X2             AND(MASK1,IA(I2))
          BX6    -X0*X3            AND(MASK2,IA(I1))
          BX7    X5+X6             OR
          LX6    B4,X7             LEFT SHIFT
          SA6    A2                STORE IN IA(I2)
          EQ     NLOOP             LOOP
*   LEFT SHIFT
 POSIT    SB5    B4-B1
          MX7    1
          AX0    B5,X7             MASK1 TO X0
          SA1    A0                ADDR.(IA(1)) TO X1
          SA4    X1                KEEP FIRST WORD IN X4
          SB6    X1                ADDR. IA(1)
          SB7    X1+B3             ADDR. IA(NW)
 PLOOP    GT     B6,B7,RETURN
          SA2    B6                ADDR.(IA(I2)) TO A2, IA(I2) TO X2
          SB6    B6+B1             ADDR(IA(I1))
          BX3    X4                TAKE LAST WORD AT END OF LOOP
          GT     B6,B7,PLOOP1
          SA3    B6                IA(I1) TO X3
 PLOOP1   BX5    -X0*X2            AND(MASK2,IA(I2))
          BX6    X0*X3             AND(MASK1,IA(I1))
          BX7    X5+X6             OR
          LX6    B4,X7             LEFT SHIFT
          SA6    A2                STORE IN IA(I2)
          EQ     PLOOP
 RETURN   SA1    SHA0
          SA0    X1                RESTORE A0
          EQ     SHLGEP            RETURN
          END
#endif
