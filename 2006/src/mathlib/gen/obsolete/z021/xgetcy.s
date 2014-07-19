*
* $Id: xgetcy.s,v 1.1.1.1 1996/04/01 15:03:22 mclareni Exp $
*
* $Log: xgetcy.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:22  mclareni
* Mathlib gen
*
*
          IDENT XGETCY
          ENTRY XGETCY
          SYSCOM
          IPARAMS
          SST
* S-P PERMETTANT DE RECUPERER LE NUMERO DE CYCLE REEL
*     IL DOIT ETRE APPELE AUSSITOT APRES UN
*        ATTACH  OU CATALOG AVEC L OPTION CY
*     CALL XGETCY(ICY)
*          ICY= NUMERO DU CYCLE EN SORTIE (ENTIER)
*
          USE /FILEM./
RCY       BSS  1
          USE *
*
FINISH    SA6  X1
XGETCY    BSS  1
          MX6  0
          SA2  RCY
          MX0  42
          BX3  X0*X2         ON NE PREND QUE PARTIE GAUCHE
          NZ   X3,FINISH     SI PARTIE GAUCHE "0 C EST PAS CY
          AX2  6
          BX6  -X0*X2
          EQ   FINISH
          END
