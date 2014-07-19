*
* $Id: dzdfio.F,v 1.1.1.1 1996/03/04 16:13:17 mclareni Exp $
*
* $Log: dzdfio.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:17  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDFIO(CHPF,IPCHPF,NSTYP,CHARIO)
      CHARACTER*(*)CHPF
      CHARACTER*1 CHARIO
      CHARACTER*4 CHFORM
      NCHPF=LEN(CHPF)
      NC=LOG10(FLOAT(NSTYP))+1
      IF(IPCHPF+NC+2.GT.NCHPF.OR.NC.GT.9)THEN
         WRITE(*,*)'IO-char too long'
         CHPF=' '
         GOTO 999
      ENDIF
      WRITE(CHFORM,'(A,I1,A)')'(I',NC,')'
      WRITE(CHPF(IPCHPF:),CHFORM)NSTYP
      CHPF(IPCHPF+NC:IPCHPF+NC)=CHARIO
      IPCHPF=IPCHPF+NC+2
 999  END
*********************************************************************
