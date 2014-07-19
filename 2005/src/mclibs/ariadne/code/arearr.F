C***********************************************************************
C $Id: arearr.F,v 1.2 1996/04/10 12:33:11 mclareni Exp $

      SUBROUTINE ARCOLI(ID,IDR)

C...Ariadne subroutine assign COLour Index

C...Assigns a colour index to dipole ID, requiring it to be different
C...IDR if IDR > 0

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arhide.f"


      ICOLI(ID)=0
      IF (PARA(26).LE.1.0.OR.QEM(ID).OR.MSTA(35).EQ.0) RETURN
      NCOL=INT(PARA(26)+0.5)
      IDP=IDI(IP1(ID))
      IDN=IDO(IP3(ID))
 100  ICOL=INT(RLU(0)*REAL(NCOL))+1
      ICOLI(ID)=ICOL
      IF (IDR.LT.0.AND.(MSTA(35).EQ.1.OR.MSTA(35).EQ.2)) THEN
        ICOLI(ID)=ICOL-1000*IDR
      ENDIF
      IF (IDR.GT.0) THEN
        IF ((MHAR(107).GE.3.OR.MHAR(107).LT.0).AND.
     $       PT2LST.GT.ABS(PHAR(112))) THEN
          ICOLI(ID)=ICOL+1000*ISTRS
        ELSE
          ICOLI(ID)=ICOL+1000*(ICOLI(IDR)/1000)
          IF (MHAR(107).GT.1.OR.(MHAR(107).EQ.1.AND.IO.EQ.1)) THEN
            IF (ICOLI(IDR).EQ.ICOLI(ID)) GOTO 100
          ENDIF
        ENDIF
      ENDIF
      IF (IDN.GT.0.AND.(.NOT.QEM(IDN))) THEN
        IF (ICOLI(IDN).EQ.ICOLI(ID)) GOTO 100
      ENDIF
      IF (IDP.GT.0.AND.(.NOT.QEM(IDP))) THEN
        IF (ICOLI(IDP).EQ.ICOLI(ID)) GOTO 100
      ENDIF

      RETURN

C**** END OF ARCOLI ****************************************************
      END
C***********************************************************************
C $Id: arearr.F,v 1.2 1996/04/10 12:33:11 mclareni Exp $

      SUBROUTINE ARSWAP(ID1,ID2)

C...ARiadne subroutine SWAP partons

C...Get the colour neighbour ICON and anti-colour neighbour ICBN of
C...parton I and the respective connecting dipoles IDCON and IDCBN.

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "arlist.f"
#include "arhide.f"

      QDONE(ID1)=.FALSE.
      QDONE(ID2)=.FALSE.
      QDUMP=.FALSE.
      PT2GG(MAXPAR-3)=-1.0
      PT2GG(MAXPAR-4)=-1.0
      NPTOT=0
      I1=IP1(ID1)
      I2=IP1(ID2)

      IS1=ISTR(ID1)
      IS2=ISTR(ID2)
      MHAR(135)=MHAR(135)+1
      IF (IS1.EQ.IS2) MHAR(136)=MHAR(136)+1
      IF (IFLOW(IS1)*IFLOW(IS2).LT.0) THEN
        IF (IFLOW(IS2).LT.0) THEN
          CALL AREVST(IS2)
        ELSE
          CALL AREVST(IS1)
        ENDIF
      ENDIF

      IP1(ID1)=I2
      IP1(ID2)=I1
      IDO(I1)=ID2
      IDO(I2)=ID1
      SDIP(ID1)=ARMAS2(I2,IP3(ID1))
      SDIP(ID2)=ARMAS2(I1,IP3(ID2))

      IF (IS1.NE.IS2) THEN
        IF (IFLOW(IS1).NE.2.AND.IFLOW(IS2).NE.2) THEN
          IL=IPL(IS1)
          IPL(IS1)=IPL(IS2)
          IPL(IS2)=IL
          I=IPF(IS1)
 100      ISTR(IDO(I))=IS1
          I=IP3(IDO(I))
          IF (I.NE.IPL(IS1)) GOTO 100
          I=IPF(IS2)
 110      ISTR(IDO(I))=IS2
          I=IP3(IDO(I))
          IF (I.NE.IPL(IS2)) GOTO 110
          CALL ARCHFL
        ELSEIF(IFLOW(IS1).EQ.2.AND.IFLOW(IS2).NE.2) THEN
          I=IPF(IS2)
 120      ISTR(IDO(I))=IS2
          I=IP3(IDO(I))
          IF (I.NE.IPL(IS2)) GOTO 120
          CALL ARREMS(IS1)
          CALL ARCHFL
        ELSEIF(IFLOW(IS1).NE.2.AND.IFLOW(IS2).EQ.2) THEN
          I=IPF(IS1)
 130      ISTR(IDO(I))=IS1
          I=IP3(IDO(I))
          IF (I.NE.IPL(IS1)) GOTO 130
          CALL ARREMS(IS2)
          CALL ARCHFL
        ELSE
          IPF(IS1)=I1
          IPL(IS1)=IP1(IDI(I1))
          ISTR(IDI(I1))=IS1
          I=IPF(IS1)
 140      ISTR(IDO(I))=IS1
          I=IP3(IDO(I))
          IF (I.NE.IPL(IS1)) GOTO 140
          CALL ARREMS(IS2)
          CALL ARCHFL
        ENDIF
        RETURN
      ENDIF

      I=IPF(IS1)
      IR1=I1
      IR2=I2
 200  IF (I.EQ.IR1) IR1=0
      IF (I.EQ.IR2) IR2=0
      I=IP3(IDO(I))
      IF (I.NE.IPL(IS1).AND.I.NE.IPF(IS1)) GOTO 200
      IF (IFLOW(IS1).EQ.2.AND.I.EQ.IPF(IS1)) IPL(IS1)=IP1(IDI(I))
      IF (I.EQ.IR1) IR1=0
      IF (I.EQ.IR2) IR2=0

      IF (MAX(IR1,IR2).EQ.0) THEN
        CALL ARCHFL
        RETURN
      ENDIF

      IF (ISTRS+1.GT.MAXSTR) CALL ARERRM('ARSWAP',8,0)

      ISTRS=ISTRS+1
      IS1=ISTRS
      IPF(IS1)=MAX(IR1,IR2)
      I=IPF(IS1)
      IPL(IS1)=IP1(IDI(I))
      ISTR(IDI(I))=IS1
      IFLOW(IS1)=2
 210  ISTR(IDO(I))=IS1
      I=IP3(IDO(I))
      IF (I.NE.IPL(IS1)) GOTO 210

      CALL ARCHFL
      RETURN

C**** END OF ARSWAP ****************************************************
      END
C***********************************************************************
C $Id: arearr.F,v 1.2 1996/04/10 12:33:11 mclareni Exp $

      SUBROUTINE ARCHFL

C...ARiadne subroutine CHeck colour FLow 

C...Checks colour flow consistency in the dipole record

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"

      DIMENSION ICP(MAXPAR),ICD(MAXDIP)
      INXT(I)=IP3(IDO(I))


      DO 100 I=1,IPART
        ICP(I)=0
 100  CONTINUE
      DO 110 I=1,IDIPS
        ICD(I)=0
 110  CONTINUE
      NPC=0

      IF (ISTRS.LE.0.OR.ISTRS.GT.MAXSTR) CALL ARERRM('ARCHFL',5,0)
C...Loop over all strings in dipole record
      DO 200 IS=1,ISTRS

C...Loop over all particles in each string
        I=IPF(IS)
 210    NPC=NPC+1
        IF (NPC.GT.IPART) CALL ARERRM('ARCHFL',5,0)
        IF (I.LE.0.OR.I.GT.IPART) CALL ARERRM('ARCHFL',5,0)
        IF (ICP(I).NE.0) CALL ARERRM('ARCHFL',5,0)
        ICP(I)=1
        IF (I.NE.IPL(IS)) THEN
          ID=IDO(I)
          IF (ID.LE.0.OR.ID.GT.IDIPS) CALL ARERRM('ARCHFL',5,0)
          IF (ICD(ID).NE.0) CALL ARERRM('ARCHFL',5,0)
          ICD(ID)=1
          I=IP3(ID)
          GOTO 210
        ENDIF
        IF (IFLOW(IS).EQ.2) THEN
          ID=IDO(I)
          IF (ID.LE.0.OR.ID.GT.IDIPS) CALL ARERRM('ARCHFL',5,0)
          IF (ICD(ID).NE.0) CALL ARERRM('ARCHFL',5,0)
          ICD(ID)=1
        ENDIF
 200  CONTINUE

      NT=1
      DO 300 I=1,IPART
        NT=NT*ICP(I)
 300  CONTINUE
      DO 310 I=1,IDIPS
        IF (.NOT.QEM(I)) NT=NT*ICD(I)
 310  CONTINUE

      IF (NT.EQ.0) CALL ARERRM('ARCHFL',5,0)
        
      RETURN

C**** END OF ARCHFL ****************************************************
      END
C***********************************************************************
C $Id: arearr.F,v 1.2 1996/04/10 12:33:11 mclareni Exp $

      SUBROUTINE AREARR

C...Ariadne subroutine REARRange colour flow.

C...Reconnects partons to alternative colour flows if this decreases
C...total 'lambda'

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arhide.f"


      IF (PARA(26).LE.1.0.OR.MSTA(35).EQ.0) RETURN
      DO 100 ID=1,IDIPS
        IF (QEM(ID)) GOTO 100
        IF (ICOLI(ID).EQ.0) CALL ARERRM('AREARR',32,0)
        SDIP(ID)=ARMAS2(IP1(ID),IP3(ID))
 100  CONTINUE

      DO 110 IS=1,ISTRS
        IF (IFLOW(IS).LT.0) CALL AREVST(IS)
 110  CONTINUE

 300  IX1SEL=0
      IX2SEL=0
      XLDIFF=0.0
      DO 200 ID1=1,IDIPS
        IF (QEM(ID1)) GOTO 200
        IA=IP1(ID1)
        IB=IP3(ID1)
        SAB=SDIP(ID1)
        AFAC=1.0
        BFAC=1.0
        IF (PHAR(107).GT.0.AND.ABS(IFL(IA)).GT.1000) AFAC=PHAR(107)
        IF (PHAR(107).GT.0.AND.ABS(IFL(IB)).GT.1000) BFAC=PHAR(107)
        ALA=0.0
        XMA=1.0
        ALB=0.0
        XMB=1.0
        IF (MHAR(106).EQ.1) THEN
          IF (QEX(IA)) THEN
            ALA=XPA(IA)
            XMA=XPMU(IA)
          ENDIF
          IF (QEX(IB)) THEN
            ALB=XPA(IB)
            XMB=XPMU(IB)
          ENDIF
        ENDIF
        SLAB=2.0*(LOG(SAB*AFAC*BFAC)+
     $       ALA*LOG(XMA)+ALB*LOG(XMB))/(2.0+ALA+ALB)
        DO 210 ID2=1,IDIPS
          IF (QEM(ID2)) GOTO 210
          IF (ID1.EQ.ID2) GOTO 210
          IF (ICOLI(ID1).NE.ICOLI(ID2)) GOTO 210
          IC=IP1(ID2)
          ID=IP3(ID2)
          SCD=SDIP(ID2)
          CFAC=1.0
          DFAC=1.0
          IF (PHAR(107).GT.0.AND.ABS(IFL(IC)).GT.1000) CFAC=PHAR(107)
          IF (PHAR(107).GT.0.AND.ABS(IFL(ID)).GT.1000) DFAC=PHAR(107)
          ALC=0.0
          XMC=1.0
          ALD=0.0
          XMD=1.0
          IF (MHAR(106).EQ.1) THEN
            IF (QEX(IC)) THEN
              ALC=XPA(IC)
              XMC=XPMU(IC)
            ENDIF
            IF (QEX(ID)) THEN
              ALD=XPA(ID)
              XMD=XPMU(ID)
            ENDIF
          ENDIF
          SAD=ARMAS2(IA,ID)
          SBC=ARMAS2(IB,IC)
          SLCD=2.0*(LOG(SCD*CFAC*DFAC)+
     $         ALC*LOG(XMC)+ALD*LOG(XMD))/(2.0+ALC+ALD)
          SLBC=2.0*(LOG(SBC*BFAC*CFAC)+
     $         ALC*LOG(XMC)+ALB*LOG(XMB))/(2.0+ALC+ALB)
          SLAD=2.0*(LOG(SAD*AFAC*DFAC)+
     $         ALA*LOG(XMA)+ALD*LOG(XMD))/(2.0+ALA+ALD)
          XLD=SLAB+SLCD-SLBC-SLAD
          IF (XLD.LE.XLDIFF) GOTO 210
          IF ((MHAR(106).EQ.-1.OR.MHAR(106).EQ.2).AND.
     $         MAX(SLAD,SLBC).GT.MIN(SLAB,SLCD)) GOTO 210
          IX1SEL=ID1
          IX2SEL=ID2
          XLDIFF=XLD
 210    CONTINUE
 200  CONTINUE
      IF (IX1SEL.EQ.0.OR.IX2SEL.EQ.0) RETURN
      CALL ARSWAP(IX1SEL,IX2SEL)
      GOTO 300

C**** END OF AREARR ****************************************************
      END

