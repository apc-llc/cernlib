C***********************************************************************
C $Id: arcrdi.F,v 1.2 1996/04/10 12:33:07 mclareni Exp $

      SUBROUTINE ARCRDI(ID,IPA1,IPA3,IS,QED)

C...ARiadne subroutine CReate DIpole

C...Creates a dipole from partons IPA1 and IPA3

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"


      IDO(IPA1)=ID
      IDI(IPA3)=ID
      IP1(ID)=IPA1
      IP3(ID)=IPA3
      ISTR(ID)=IS
      QDONE(ID)=.FALSE.
      QEM(ID)=QED
      ICOLI(ID)=0

      RETURN

C**** END OF ARCRDI ****************************************************
      END
