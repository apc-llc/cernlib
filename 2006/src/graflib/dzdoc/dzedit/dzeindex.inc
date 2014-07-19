*
* $Id: dzeindex.inc,v 1.1.1.1 1996/03/04 16:13:19 mclareni Exp $
*
* $Log: dzeindex.inc,v $
* Revision 1.1.1.1  1996/03/04 16:13:19  mclareni
* Dzdoc/Zebpack
*
*
*
* dzeindex.inc
*
      PARAMETER (MAXBNK=2000)
      CHARACTER*4 CINDEX(MAXBNK)
      COMMON/DZEIDC/CINDEX
      INTEGER IORD(MAXBNK),IPINDX(MAXBNK)
      COMMON/DZEIDI/IORD,IPINDX
 
