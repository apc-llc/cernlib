C***********************************************************************
C $Id: araddg.F,v 1.2 1996/04/10 12:33:00 mclareni Exp $

      SUBROUTINE ARADDG(ID,ICLOSE)

C...ARiadne subroutine ADD Gluon

C...Adds a gluon entry between the partons in dipole ID thus creating 
C...a new dipole

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"

      INXT(I)=IDO(IP3(I))
      IPRV(I)=IDI(IP1(I))


C...Allocate new gluon and new dipole at postitons IPART+1 and IDIPS+1
C...if there is space left.
      IPART=IPART+1
      IDIPS=IDIPS+1
      IF (IPART.GE.MAXPAR-10) CALL ARERRM('ARADDG',6,0)
      IF (IDIPS.GE.MAXDIP-10) CALL ARERRM('ARADDG',7,0)

C...Set properties of new gluon
      DO 100 I=1,5
        BP(IPART,I)=0.0
 100  CONTINUE
      IFL(IPART)=21
      QEX(IPART)=.FALSE.
      XPMU(IPART)=0.0
      XPA(IPART)=0.0
      QQ(IPART)=.FALSE.
      INQ(IPART)=0
      IDI(IPART)=ID
      IDO(IPART)=IDIPS
      PT2GG(IPART)=PT2IN(ID)

C...Set properties of new dipole
      IP1(IDIPS)=IPART
      IP3(IDIPS)=IP3(ID)
      QDONE(IDIPS)=.FALSE.
      QEM(IDIPS)=.FALSE.
      ISTR(IDIPS)=ISTR(ID)
      PT2IN(IDIPS)=PT2IN(ID)

C...Fix pointers for old dipole
      IP3(ID)=IPART
      IDI(IP3(IDIPS))=IDIPS
      IF (IPRV(ID).NE.0) QDONE(IPRV(ID))=.FALSE.
      QDONE(ID)=.FALSE.
      IF (INXT(IDIPS).NE.0) QDONE(INXT(IDIPS))=.FALSE.

      IS=ISTR(ID)
      IF (IFLOW(IS).EQ.2) THEN
        IPF(IS)=IP3(ID)
        IPL(IS)=IP1(ID)
      ENDIF

C...Fix colour assignment
      ISCOL=ICOLI(ID)/1000
      IF (ICLOSE.EQ.1) THEN
        ICOLI(IDIPS)=ICOLI(ID)
        CALL ARCOLI(ID,-ISCOL)
      ELSE
        CALL ARCOLI(IDIPS,-ISCOL)
      ENDIF

      RETURN

C**** END OF ARADDG ****************************************************
      END
C***********************************************************************
C $Id: araddg.F,v 1.2 1996/04/10 12:33:00 mclareni Exp $

      SUBROUTINE ARREMG(IGI)

C...ARiadne subroutine REMove Gluon

C...Removes the gluon entry IG and reconnects neighboring dipoles

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"

C...Check that we don't have a gluon ring...
      IG=IGI
      IF (IFLOW(ISTR(IDO(IG))).EQ.2.AND.
     $     IP3(IDO(IG)).EQ.IP1(IDI(IG))) RETURN

C...First change pointers making IG and one neighboring dipole orphans
      IDREM=IDO(IG)
      IP=IP3(IDREM)
      ID=IDI(IG)
      IP3(ID)=IP
      IDI(IP)=ID
      IDP=IDI(IG)
      IF (IDP.GT.IDREM) IDP=IDP-1

C...Purge the parton vector
      DO 100 IP=IG+1,IPART
        I=IP-1
        DO 110 J=1,5
          BP(I,J)=BP(IP,J)
 110    CONTINUE
        IFL(I)=IFL(IP)
        QEX(I)=QEX(IP)
        QQ(I)=QQ(IP)
        IDI(I)=IDI(IP)
        IDO(I)=IDO(IP)
        INO(I)=INO(IP)
        INQ(I)=INQ(IP)
        XPMU(I)=XPMU(IP)
        XPA(I)=XPA(IP)
        PT2GG(I)=PT2GG(IP)
 100  CONTINUE
      IPART=IPART-1

C...Purge the dipole vector
      DO 200 ID=IDREM+1,IDIPS
        I=ID-1
        BX1(I)=BX1(ID)
        BX3(I)=BX3(ID)
        PT2IN(I)=PT2IN(ID)
        SDIP(I)=SDIP(ID)
        IP1(I)=IP1(ID)
        IP3(I)=IP3(ID)
        AEX1(I)=AEX1(ID)
        AEX1(I)=AEX3(ID)
        QDONE(I)=QDONE(ID)
        QEM(I)=QEM(ID)
        IRAD(I)=IRAD(ID)
        ISTR(I)=ISTR(ID)
        ICOLI(I)=ICOLI(ID)
 200  CONTINUE
      IDIPS=IDIPS-1

C...Reset changed pointers

      DO 300 IP=1,IPART
        IF (IDO(IP).GE.IDREM) IDO(IP)=IDO(IP)-1
        IF (IDI(IP).GE.IDREM) IDI(IP)=IDI(IP)-1
 300  CONTINUE
      DO 310 ID=1,IDIPS
        IF (IP3(ID).GE.IG) IP3(ID)=IP3(ID)-1
        IF (IP1(ID).GE.IG) IP1(ID)=IP1(ID)-1
 310  CONTINUE
      DO 320 IS=1,ISTRS
        IF (IPF(IS).GE.IG) IPF(IS)=IPF(IS)-1
        IF (IPL(IS).GE.IG) IPL(IS)=IPL(IS)-1
 320  CONTINUE

C...Fix up colour indices
      IDN=IDO(IP3(IDP))
      IF (IDN.GT.0) THEN
        IF (ICOLI(IDN).EQ.ICOLI(IDP)) THEN
          ISCOL=ICOLI(IDN)/1000
          ICOLI(IDN)=0
          ICOLI(IDP)=0
          CALL ARCOLI(IDN,-ISCOL)
          CALL ARCOLI(IDP,-ISCOL)
        ENDIF
      ENDIF

      RETURN

C**** END OF ARREMG ****************************************************
      END
C***********************************************************************
C $Id: araddg.F,v 1.2 1996/04/10 12:33:00 mclareni Exp $

      SUBROUTINE ARJOQQ(IQ1,IQ2)

C...ARiadne subroutine JOin QQbar pair

C...Join quark and anti-quark entry IQ1 and IQ2 into a gluon entry
C...located in MIN(IQ1,IQ2). Disregard flavour consistency

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"


      IG=MIN(IQ1,IQ2)
      IREM=MAX(IQ1,IQ2)

C...First remove any EM-dipoles
      IF (IDI(IREM).GT.0) THEN
        IF (QEM(IDI(IREM))) CALL ARREMD(IDI(IREM))
      ENDIF
      IF (IDO(IREM).GT.0) THEN
        IF (QEM(IDO(IREM))) CALL ARREMD(IDO(IREM))
      ENDIF
      IF (IDI(IG).GT.0) THEN
        IF (QEM(IDI(IG))) CALL ARREMD(IDI(IG))
      ENDIF
      IF (IDO(IG).GT.0) THEN
        IF (QEM(IDO(IG))) CALL ARREMD(IDO(IG))
      ENDIF

C...Check that quarks not directly connected and then join them
      IF (IDI(IG).GT.0) THEN
        IF (IDO(IG).GT.0) RETURN
        IF (IDI(IREM).GT.0) CALL AREVST(ISTR(IDI(IREM)))
        CALL ARJOST(ISTR(IDI(IG)),ISTR(IDO(IREM)),IG,IREM)
        IP1(IDO(IREM))=IG
        IDO(IG)=IDO(IREM)
        IDO(IREM)=0
      ELSE
        IF (IDO(IG).LE.0) RETURN
        IF (IDO(IREM).GT.0) CALL AREVST(ISTR(IDO(IREM)))
        CALL ARJOST(ISTR(IDO(IG)),ISTR(IDI(IREM)),IG,IREM)
        IP3(IDI(IREM))=IG
        IDI(IG)=IDI(IREM)
        IDI(IREM)=0
      ENDIF

C...Remove quark entry
      CALL ARREMP(IREM)
      QQ(IG)=.FALSE.
      IFL(IG)=21
      PT2GG(IG)=0.0
      INQ(IG)=0

      IF (ICOLI(IDI(IG)).EQ.ICOLI(IDO(IG))) THEN
        ISCOL=ICOLI(IDI(IG))/1000
        ICOLI(IDI(IG))=0
        ICOLI(IDO(IG))=0
        CALL ARCOLI(IDI(IG),-ISCOL)
        CALL ARCOLI(IDO(IG),-ISCOL)
      ENDIF

      RETURN

C**** END OF ARJOQQ ****************************************************
      END
C***********************************************************************
C $Id: araddg.F,v 1.2 1996/04/10 12:33:00 mclareni Exp $

      SUBROUTINE ARREMP(IREM)

C...ARiadne subroutine REMove Parton

C...Remove parton entry from /ARPART/ and purge event record

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"


C...Purge the parton vector
      DO 100 IP=IREM+1,IPART
        I=IP-1
        DO 110 J=1,5
          BP(I,J)=BP(IP,J)
 110    CONTINUE
        IFL(I)=IFL(IP)
        QEX(I)=QEX(IP)
        QQ(I)=QQ(IP)
        IDI(I)=IDI(IP)
        IDO(I)=IDO(IP)
        INO(I)=INO(IP)
        INQ(I)=INQ(IP)
        XPMU(I)=XPMU(IP)
        XPA(I)=XPA(IP)
        PT2GG(I)=PT2GG(IP)
 100  CONTINUE

C...Fix pointers for special entries
      DO 120 IP=MAXPAR-4,MAXPAR-3
        IF (.NOT.QQ(IP)) GOTO 120
        IF (INQ(IP).EQ.IREM) THEN
          INQ(IP)=0
        ELSEIF (INQ(IP).GT.IREM.AND.INQ(IP).LE.IPART) THEN
          INQ(IP)=INQ(IP)-1
        ENDIF
        IF (IDI(IP).EQ.IREM) THEN
          IDI(IP)=0
        ELSEIF (IDI(IP).GT.IREM.AND.IDI(IP).LE.IPART) THEN
          IDI(IP)=IDI(IP)-1
        ENDIF
 120  CONTINUE

C...Reset changed pointers

      DO 200 ID=1,IDIPS
        IF (IP1(ID).EQ.IREM) THEN
          IP1(ID)=0
        ELSEIF (IP1(ID).GT.IREM.AND.IP1(ID).LE.IPART) THEN
          IP1(ID)=IP1(ID)-1
        ENDIF
        IF (IP3(ID).EQ.IREM) THEN
          IP3(ID)=0
        ELSEIF (IP3(ID).GT.IREM.AND.IP3(ID).LE.IPART) THEN
          IP3(ID)=IP3(ID)-1
        ENDIF
 200  CONTINUE

      DO 210 IS=1,ISTRS
        IF (IPF(IS).GE.IREM.AND.IPF(IS).LE.IPART) IPF(IS)=IPF(IS)-1
        IF (IPL(IS).GE.IREM.AND.IPL(IS).LE.IPART) IPL(IS)=IPL(IS)-1
 210  CONTINUE

      IPART=IPART-1

      RETURN

C**** END OF ARREMP ****************************************************
      END
C***********************************************************************
C $Id: araddg.F,v 1.2 1996/04/10 12:33:00 mclareni Exp $

      SUBROUTINE ARREMD(IREM)

C...ARiadne subroutine REMove Dipole

C...Remove dipole entry from /ARDIPS/ and purge event record

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"


      IDP=-1
      IF (IP1(IREM).GT.0) IDP=IDI(IP1(IREM))
      IDN=-1
      IF (IP3(IREM).GT.0) IDN=IDO(IP3(IREM))
      IF (IDP.GT.IREM) IDP=IDP-1
      IF (IDN.GT.IREM) IDN=IDN-1

C...Purge the dipole vector
      DO 100 ID=IREM+1,IDIPS
        I=ID-1
        BX1(I)=BX1(ID)
        BX3(I)=BX3(ID)
        PT2IN(I)=PT2IN(ID)
        SDIP(I)=SDIP(ID)
        IP1(I)=IP1(ID)
        IP3(I)=IP3(ID)
        AEX1(I)=AEX1(ID)
        AEX3(I)=AEX3(ID)
        QDONE(I)=QDONE(ID)
        QEM(I)=QEM(ID)
        IRAD(I)=IRAD(ID)
        ISTR(I)=ISTR(ID)
        ICOLI(I)=ICOLI(ID)
 100  CONTINUE

      DO 200 IP=1,IPART
        IF (IDO(IP).EQ.IREM) THEN
          IDO(IP)=0
        ELSEIF (IDO(IP).GT.IREM.AND.IDO(IP).LE.IDIPS) THEN
          IDO(IP)=IDO(IP)-1
        ENDIF
        IF (IDI(IP).EQ.IREM) THEN
          IDI(IP)=0
        ELSEIF (IDI(IP).GE.IREM.AND.IDI(IP).LE.IDIPS) THEN
          IDI(IP)=IDI(IP)-1
        ENDIF
 200  CONTINUE

      IDIPS=IDIPS-1

      IF (IDP.GT.0.AND.IDN.GT.0) THEN
        IF (ICOLI(IDP).EQ.ICOLI(IDN)) THEN
          ISCOL=ICOLI(IDP)/1000
          ICOLI(IDP)=0
          ICOLI(IDN)=0
          CALL ARCOLI(IDP,-ISCOL)
          CALL ARCOLI(IDN,-ISCOL)
        ENDIF
      ENDIF

      RETURN

C**** END OF ARREMD ****************************************************
      END
C***********************************************************************
C $Id: araddg.F,v 1.2 1996/04/10 12:33:00 mclareni Exp $

      SUBROUTINE AREVST(ISIN)

C...Ariadne subroutine REVerse colour flow of STring entry

C...Reverse the colour flow of string entry ISIN in /ARSTRS/

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"


C...Purely glounic strings have no determined flow
      IF (IFLOW(ISIN).EQ.2) RETURN

C...The first shall be the last
      IS=ISIN
      IP=IPF(IS)
 100  ICO=IDO(IP)
      IDO(IP)=IDI(IP)
      IDI(IP)=ICO
      IF (ICO.GT.0) THEN
        IPS=IP3(ICO)
        IP3(ICO)=IP1(ICO)
        IP1(ICO)=IPS
        BX=BX1(ICO)
        BX1(ICO)=BX3(ICO)
        BX3(ICO)=BX
        AEX=AEX1(ICO)
        AEX1(ICO)=AEX3(ICO)
        AEX3(ICO)=AEX
        IF (ABS(IRAD(ICO)).LT.10) IRAD(ICO)=-IRAD(ICO)
      ENDIF
      IF (IP.NE.IPL(IS)) THEN
        IP=IPS
        GOTO 100
      ENDIF

      IFLOW(IS)=-IFLOW(IS)
      IP=IPF(IS)
      IPF(IS)=IPL(IS)
      IPL(IS)=IP

      RETURN

C**** END OF AREVST ****************************************************
      END
C***********************************************************************
C $Id: araddg.F,v 1.2 1996/04/10 12:33:00 mclareni Exp $

      SUBROUTINE ARJOST(IS1,IS2,IPA1,IPA2)

C...ARiadne subroutine JOin two STring entries

C...Join the string entries IS1 and IS2 in the ends IP1 and IP2

#include "arimpl.f"
#include "ardips.f"
#include "arstrs.f"


      IS=IS1
      ISREM=IS2
      IP=IPA1
      IPREM=IPA2

C...If the strings are the same we make a purely gluonic string
      IF (IS.EQ.ISREM) THEN
        IFLOW(IS)=2
        IPF(IS)=IP
        IPL(IS)=IP
        RETURN
      ENDIF

      IF (IP.EQ.IPF(IS)) THEN
        IF (IPREM.EQ.IPF(ISREM)) CALL ARERRM('ARJOST',5,0)
        IPF(IS)=IPF(ISREM)
      ELSE
        IF (IP.NE.IPL(IS).OR.IPREM.EQ.IPL(ISREM))
     $       CALL ARERRM('ARJOST',5,0)
        IPL(IS)=IPL(ISREM)
      ENDIF

      DO 100 ID=1,IDIPS
        IF (ISTR(ID).EQ.ISREM) ISTR(ID)=IS
 100  CONTINUE

      DO 110 IS=ISREM,ISTRS-1
        IPF(IS)=IPF(IS+1)
        IPL(IS)=IPL(IS+1)
        IFLOW(IS)=IFLOW(IS+1)
 110  CONTINUE

      ISTRS=ISTRS-1

      DO 120 ID=1,IDIPS
        IF (ISTR(ID).GT.ISREM) ISTR(ID)=ISTR(ID)-1
 120  CONTINUE

      RETURN

C**** END OF ARJOST ****************************************************
      END
C***********************************************************************
C $Id: araddg.F,v 1.2 1996/04/10 12:33:00 mclareni Exp $

      SUBROUTINE ARPADD(IP,NP,IPV)

C...ARiadne subroutine Pointer ADD

C...Add pointer IP to vector IPV(NP)

#include "arimpl.f"

      DIMENSION IPV(NP+1)

      IF (IP.GT.0) GOTO 900
      DO 100 I=1,NP
        IF (IPV(I).EQ.ABS(IP)) RETURN
 100  CONTINUE

 900  NP=NP+1
      IPV(NP)=ABS(IP)

      RETURN

C**** END OF ARPADD ****************************************************
      END
C***********************************************************************
C $Id: araddg.F,v 1.2 1996/04/10 12:33:00 mclareni Exp $

      SUBROUTINE ARPADO(IP,NP,IPV)

C...ARiadne subroutine Pointer ADd to Ordered list

C...Add pointer IP to ordered vector IPV(NP)

#include "arimpl.f"

      DIMENSION IPV(NP+1)

      IPV(NP+1)=0
      DO 100 I=1,NP
        IF (IPV(I).EQ.IP) RETURN
        IF (IP.LT.IPV(I)) THEN
          CALL ARPINS(IP,I,NP,IPV)
          RETURN
        ENDIF
 100  CONTINUE

      CALL ARPINS(IP,NP+1,NP,IPV)

      RETURN

C**** END OF ARPADO ****************************************************
      END
C***********************************************************************
C $Id: araddg.F,v 1.2 1996/04/10 12:33:00 mclareni Exp $

      SUBROUTINE ARPINS(IP,INP,NP,IPV)

C...ARiadne subroutine Pointer INSert

C...Insert pointer IP to vector IPV(NP) in position I

#include "arimpl.f"

      DIMENSION IPV(NP+1)

      DO 100 I=NP,INP,-1
        IPV(I+1)=IPV(I)
 100  CONTINUE
      IPV(INP)=IP

      NP=MAX(NP+1,INP)

      RETURN

C**** END OF ARPINS ****************************************************
      END
C***********************************************************************
C $Id: araddg.F,v 1.2 1996/04/10 12:33:00 mclareni Exp $

      SUBROUTINE ARPREM(IP,NP,IPV)

C...ARiadne subroutine Pointer REMove

C...Remove pointer IP from vector IPV(NP) and purge the vector.

#include "arimpl.f"

      DIMENSION IPV(NP)

      NFOUND=0
      DO 100 I=1,NP
        IF (NFOUND.GT.0) IPV(I-NFOUND)=IPV(I)
        IF (IPV(I).EQ.IP) NFOUND=NFOUND+1
 100  CONTINUE

      NP=NP-NFOUND

      RETURN

C**** END OF ARPREM ****************************************************
      END
C***********************************************************************
C $Id: araddg.F,v 1.2 1996/04/10 12:33:00 mclareni Exp $

      SUBROUTINE ARREMS(IREM)

C...ARiadne subroutine REMove String 

C...Remove String entry from /ARSTRS/ and purge event record

#include "arimpl.f"
#include "ardips.f"
#include "arstrs.f"


C...Purge the dipole vector
      DO 100 IS=IREM+1,ISTRS
        I=IS-1
        IFLOW(I)=IFLOW(IS)
        IPF(I)=IPF(IS)
        IPL(I)=IPL(IS)
 100  CONTINUE

      ISTRS=ISTRS-1

      DO 110 ID=1,IDIPS
        IF (ISTR(ID).EQ.IREM) ISTR(ID)=0
        IF (ISTR(ID).GT.IREM) ISTR(ID)=ISTR(ID)-1
 110  CONTINUE

      RETURN

C**** END OF ARREMS ****************************************************
      END
