*
* $Id: dbrinf.inc,v 1.1.1.1 1996/02/28 16:23:38 mclareni Exp $
*
* $Log: dbrinf.inc,v $
* Revision 1.1.1.1  1996/02/28 16:23:38  mclareni
* Hepdb, cdlib, etc
*
*
*
* dbrinf.inc
*
* - - - - - -/DBRINF/- - - - - - - - - - - - - - - - - -
      COMMON /DBRTIT/  DBRTYPE,DBRPAKM,DBRPATH
      CHARACTER        DBRTYPE*4,DBRPAKM*4,DBRPATH*40
*
      COMMON /DBRKYS/  NDBRKY,NDBRPR,KEYDBR(20)
      INTEGER          NDBRKY,NDBRPR,KEYDBR
*
      COMMON /DBRLNK/  IDBRDIV,LDBRKY,LDBRDA
      INTEGER          IDBRDIV,LDBRKY,LDBRDA
*
      COMMON /DBRINP/  LUDBRIN,NLDBRIN,DBRIFIL
      INTEGER          LUDBRIN,NLDBRIN
      CHARACTER*60     DBRIFIL
* - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c\* - - - - - -/DBRINF/- - - - - - - - - - - - - - - - - -
c\      COMMON /DBRINP/  LUDBRIN,NLDBRIN,DBRIFIL
c\      INTEGER          LUDBRIN,NLDBRIN
c\      CHARACTER*60     DBRIFIL
c\*
c\      COMMON /DBRINF/  IDBRTYP,IDBRMOD,LENGDBR,DBRTYPE,DBRPATH
c\      INTEGER          IDBRTYP,IDBRMOD,LENGDBR
c\      CHARACTER        DBRTYPE*4,DBRPATH*40
c\*
c\      COMMON /DBRTIT/  DBRTIT
c\      CHARACTER        DBRTIT*72
c\*
c\      COMMON /DBRBUF/  IDBFDV,LDBRBF
c\      INTEGER          IDBFDV,LDBRBF
c\*
c\      COMMON /DBRKYS/  NDBRKY,NDBRPR,KEYDBR(20),KINDBR(20),KMSDBR(20)
c\      INTEGER          NDBRKY,NDBRPR,KEYDBR,KINDBR,KMSDBR
c\*
c\      COMMON /DBRLNK/  LDBRKY,LDBRDA
c\      INTEGER          LDBRKY,LDBRDA
c\* - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
