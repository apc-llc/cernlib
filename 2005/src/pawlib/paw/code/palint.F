*
* $Id: palint.F,v 1.2 1996/09/11 14:59:32 couet Exp $
*
* $Log: palint.F,v $
* Revision 1.2  1996/09/11 14:59:32  couet
* - Hgetnt and Hgetn2 (old qp routines) are now replaced by hntld
*
* Revision 1.1.1.1  1996/03/01 11:38:41  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/00 18/06/93  14.40.33  by  Rene Brun
*-- Author :    J.LE FOLL   05/04/90
      SUBROUTINE PALINT
C
C     Action routine for the command /NTUPLE/LINTRA
C     Ntuple reduction variable
C
C
C     Palint adapted from pascan
C
C           Author   J.Le Foll
C
#include "paw/pawcom.inc"
#include "paw/pcpatl.inc"
#include "paw/quest.inc"
      CHARACTER*8 CHOPT
C
      CALL KUPATL(CHPATL,NPAR)
      CALL KUGETC(CHID,N)
      CALL HNTLD(CHID)
      IF(IQUEST(1).LT.0)RETURN
      CALL KUGETC(CHOPT,NCHOPT)
      CALL KUGETI(NEVENT)
      CALL KUGETI(IFROM)
      CALL KUGETI(NVARS)
      INORM=INDEX(CHOPT,'N')
      IOPTP=INDEX(CHOPT,'P')
      CALL KUALFA
      IDN=ID
      ITO=IFROM+NEVENT-1
      IF(NPAR.LT.4) NVARS=0
      CALL PALIN1(IDN,NCH,IFROM,ITO,NVARS,INORM,IOPTP,0)
      CALL HRSTD
C
      END

