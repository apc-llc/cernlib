*
* $Id: ranf.s,v 1.1.1.1 1996/02/15 17:48:31 mclareni Exp $
*
* $Log: ranf.s,v $
* Revision 1.1.1.1  1996/02/15 17:48:31  mclareni
* Kernlib
*
*
#include "kernnum/linkg900.inc"
          MACRO
&LABEL    RANDATA
          DS        0D
&LABEL    DC        X'402BC68CFE166D00' SEED = 0.2BC68CFE166D00
          DC        X'4C2875A2E7B17500' MULT =    2875 A2E7 B175.00
          DC        X'4E10000000000000' NORM = 10 0000 0000 0000.
          MEND
          MACRO
&LABEL    RANF      &DATA
&LABEL    LD        0,&DATA             F0 = 0.2BC68CFE166D00
          MXD       0,&DATA+8           F0 =      123456789ABC.DE
*                                    F2 = .(00)123456789A0000 (EXP=-2)
          LD        4,&DATA+16       F4 = 10 0000 0000 0000.  (EXP=14)
          LD        6,=D'0'             F6 = 0
          AXR       0,4            F0 = 10 1234 5678 9ABC.
*                                  F2 =             .DE 1234 5678 9A00
          STD       2,&DATA             SEED = .DE 1234 5678 9A00
          LDR       0,2                 F0 = NEW SEED (NOT NORMALIZED)
          ADR       0,6                 F0 = .DE 1234 5678 9A00 = RANF
          MEND
          GBLC      &NAME
&NAME     SETC      'RANF  '
&NAME     START
&NAME     AMODE     ANY
&NAME     RMODE     ANY
          ENTRY     DRANF,G900GT,G900ST
REGLIM    EQU       1
          PROLOG    REGLIM
          RANF      DATA
          EXIT      REGLIM
&NAME     SETC      'DRANF '
&NAME     DS        0D                  DRANF(DUMMY)
          PROLOG    REGLIM
          RANF      DATA
          EXIT      REGLIM
&NAME     SETC      'G900GT'
&NAME     DS        0D                  G900GT(DUMMY)
          PROLOG    REGLIM
          LD        0,DATA              F0 = SEED
          LM        0,1,DATA            R0 = 402BC68C
*                                       R1 = FE166D00
          N         0,=X'00FFFFFF'      R0 = 002BC68C
          SRL       1,8                 R1 = 00FE166D
          STM       0,1,TEMP
          LD        0,TEMP              F0 = 002BC68C 00FE166D
          EXIT      REGLIM
&NAME     SETC      'G900ST'
&NAME     DS        0D                  G900ST(SEED)
          PROLOG    REGLIM
          L         1,0(1)              R1 = (SEED) + 80000000
          N         1,=X'7FFFFFFF'      R1 = (SEED)
          LD        0,0(1)              F0 = SEED = 002BC68C 00FE166D
          STD       0,TEMP
          LM        0,1,TEMP            R0 = 002BC68C
*                                       R1 = 00FE166D
          A         0,=X'40000000'      R0 = 402BC68C
          SLL       1,8                 R1 = FE166D00
          STM       0,1,DATA            SEED = 402BC68C FE166D00
          EXIT      REGLIM
DATA      RANDATA
TEMP      DS        1D
          END
