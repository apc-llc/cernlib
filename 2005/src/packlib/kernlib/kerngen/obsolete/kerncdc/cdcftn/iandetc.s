*
* $Id: iandetc.s,v 1.1.1.1 1996/02/15 17:51:20 mclareni Exp $
*
* $Log: iandetc.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:20  mclareni
* Kernlib
*
*
          IDENT       IAND
*
* CERN PROGLIB# M441    IANDETC         .VERSION KERNCDC  2.18  870602
* ORIG. ?
*
*     COMPASS VERSIONS OF  IAND, IOR, IEOR, NOT
*
          ENTRY       IAND
          ENTRY       IOR
          ENTRY       IEOR
          ENTRY       NOT
*
*     FUNCTION IAND(I,J)
*
          VFD         24/4HIAND,36/IAND
 IAND     DATA        0
          SA2         A1+1
          SA2         X2
          SA1         X1
          BX6         X1*X2
          EQ          B0,B0,IAND
*
*     FUNCTION IOR(I,J)
*
          VFD         18/3HIOR,42/IOR
 IOR      DATA        0
          SA2         A1+1
          SA2         X2
          SA1         X1
          BX6         X1+X2
          EQ          B0,B0,IOR
*
*     FUNCTION IEOR(I,J)
*
          VFD         24/4HIEOR,36/IEOR
 IEOR     DATA        0
          SA2         A1+1
          SA2         X2
          SA1         X1
          BX6         X1-X2
          EQ          B0,B0,IEOR
*
*     FUNCTION NOT(I)
*
          VFD         18/3HNOT,42/NOT
 NOT      DATA        0
          SA1         X1
          BX6         -X1
          EQ          B0,B0,NOT
      END
