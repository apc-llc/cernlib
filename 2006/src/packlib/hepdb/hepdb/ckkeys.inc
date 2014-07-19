*
* $Id: ckkeys.inc,v 1.1.1.1 1996/02/28 16:24:05 mclareni Exp $
*
* $Log: ckkeys.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:05  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_CKKEYS_INC
#define CERNLIB_HEPDB_CKKEYS_INC
*
*
* ckkeys.inc
*
      INTEGER          TENCK
      PARAMETER       (MXDMCK=90, MXKYCK=10000, TENCK=10, MXKPCK=1000)
      PARAMETER       (NOF1CK=10, NOF2CK=10, NOF3CK=6)
      PARAMETER       (NOFSCK=5,  NOFECK=5)
* For DBL3
*     PARAMETER       (NOF1CK=2, NOF2CK=5, NOF3CK=3)
*
      COMMON /CKKEYS/ ICONCK(MXDMCK), INDKCK(MXKYCK), IOTYCK(MXDMCK)
     +              , IPURCK(MXKYCK), KEY1CK(MXKYCK), KEYNCK(MXDMCK)
     +              , KEYVCK(MXDMCK), KYP1CK(MXDMCK), KYP2CK(MXDMCK)
     +              , KYVMCK(MXDMCK), KEY7CK, NKEYCK, NSKPCK, NSYSCK
     +              , NWKYCK
      DIMENSION       RKY1CK(MXKYCK)
      EQUIVALENCE     (KEY1CK(1), RKY1CK(1))
*
      COMMON /CKTAGS/ CHTGCK(TENCK), CTAGCK(MXDMCK), CHFTCK
      CHARACTER       CHFTCK*10, CHTGCK*8, CTAGCK*8
*

#endif
