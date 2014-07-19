*
* $Id: entftn.inc,v 1.1.1.1 1996/04/01 15:03:01 mclareni Exp $
*
* $Log: entftn.inc,v $
* Revision 1.1.1.1  1996/04/01 15:03:01  mclareni
* Mathlib gen
*
*
* CDC MACRO CONVERTING PARAMETER LISTS FROM CERN TO CDC SCOPE.
*
* entftn.inc
*
#if (defined(CERNLIB_QF2C))&&(!defined(CERNLIB_LINUX))
 ENTFTN   MACRO     (NM,NOPAR,REG)
          LOCAL     Z
 NM       BSSZ      1
 Z1Z1Z    IFNE      *F,1
          EQ        *+3
          BSSZ      2
          SA2       NM
          SA3       =O5302046000
          BX6       X2+X3
          LX6       30
          SA6       NM+2
          SA3       =O51200000000400000000
          SX4       NM+2
          IX3       X3+X4
          SX4       X4+1
          LX4       30
          BX6       X3+X4
          SA6       A2
          SX6       A0
          SB7       A1
          SA6       NM+3
 Z1Z1     SET       1                   DIGITSELECTOR
          DUP       NOPAR
          ZR        X1,Z
 Z1Z1Z1   IFLT      Z1Z1,7              IF FIRST SIX
          SB.Z1Z1   X1
 Z1Z1Z1   ELSE     2
          LX6       X1
          SA6       NM-NOPAR-2+Z1Z1
 Z1Z1Z1   ENDIF
 Z1Z1     SET       Z1Z1+1
 Z1Z      IFGE      NOPAR,Z1Z1
          SA1       A1+1                LOAD NEXT
 Z1Z      ENDIF
          ENDD
 Z        BSS       0
Z1Z1Z11   IFC       NE,/REG//
Z1Z1      SET    1
Z1Z1Z1Z   IFC    EQ,/REG/X1/
Z1Z1      SET    2
          BX2    X1
Z1Z1Z1Z   ENDIF
          S_REG     A1-B7
          IFNE      NOPAR,0
          ZR    X.Z1Z1,*+2-$/46
          S_REG     REG+1
          BSS       0
          ENDIF
          ENDM
#endif
