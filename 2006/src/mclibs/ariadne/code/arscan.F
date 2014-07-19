C***********************************************************************
C $Id: arscan.F,v 1.2 1996/04/10 12:33:38 mclareni Exp $

      SUBROUTINE ARSCAN(NSTART,NEND,NR,IR)

C...ARiadne subroutine SCAN the event record

C...Parse through the /LUJETS/ event record to find un-cascaded
C...strings and put them in the Ariadne event record

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arhide.f"
#include "lujets.f"

      DIMENSION IR(NR)


      IDIR=0
      IPART=0
      IDIPS=0
      ISTRS=0
      PT2LST=PARA(40)
      QQ(MAXPAR-3)=.FALSE.
      QQ(MAXPAR-4)=.FALSE.
      IMF=0
      IML=0
      PHAR(131)=0.0
      PHAR(132)=0.0

C...Loop over entries in /LUJETS/ to be considered
      DO 100 I=NSTART,NEND

C...If IDIR=0 there is no current string so skip all entries which
C...are not the begining of a string (K(I,1)=2) otherwise copy
C...parton to dipole record
        IF (IDIR.EQ.0) THEN
          IF (K(I,1).NE.2) THEN
            K(I,4)=MAX(K(I,4),0)
            GOTO 100
          ENDIF
          CALL ARGTYP(I,ITYP)
          IF (MHAR(133).GE.1.AND.ITYP.EQ.2.AND.K(I,3).EQ.0) THEN
            K(I,4)=MAX(K(I,4),0)
            PHAR(131)=PHAR(131)+P(I,3)+P(I,4)
            PHAR(132)=PHAR(132)-P(I,3)+P(I,4)
            IF (K(I+1,1).EQ.1) THEN
              PHAR(131)=PHAR(131)+P(I+1,3)+P(I+1,4)
              PHAR(132)=PHAR(132)-P(I+1,3)+P(I+1,4)
            ENDIF
            GOTO 100
          ENDIF            
          IF (MHAR(133).GE.3.AND.K(I,3).EQ.0) THEN
            K(I,4)=MAX(K(I,4),0)
            PHAR(131)=PHAR(131)+P(I,3)+P(I,4)
            PHAR(132)=PHAR(132)-P(I,3)+P(I,4)
            IF (K(I+1,1).EQ.1) THEN
              PHAR(131)=PHAR(131)+P(I+1,3)+P(I+1,4)
              PHAR(132)=PHAR(132)-P(I+1,3)+P(I+1,4)
            ENDIF
            GOTO 100
          ENDIF            
          IF (ITYP.EQ.0) CALL ARERRM('ARPARS',1,I)
          IF (MSTA(29).EQ.1.AND.ITYP.EQ.2) THEN
            K(I,4)=MAX(K(I,4),0)
            GOTO 100
          ENDIF
          IF (IMF.EQ.0) IMF=I
          IDIR=ITYP
          IPART=IPART+1
          IPFST=IPART
          ISTRS=ISTRS+1
          CALL ARCOPA(I,IPART,ITYP)
          DO 200 J=1,NR
            IF (IR(J).EQ.I) IR(J)=-IPART
 200      CONTINUE
        ELSE

          DO 210 J=1,NR
            IF (IR(J).EQ.I) IR(J)=-IPART-1
 210      CONTINUE

C...If in a string, copy parton and create a dipole. Error if
C...colour singlets of triplets are found
          IF (K(I,1).EQ.2) THEN
            CALL ARGTYP(I,ITYP)
            IF (ABS(ITYP).EQ.1) CALL ARERRM('ARPARS',2,I)
            IF (ABS(ITYP).EQ.0) CALL ARERRM('ARPARS',1,I)
            IPART=IPART+1
            IDIPS=IDIPS+1
            CALL ARCOPA(I,IPART,ITYP)
            CALL ARCRDI(IDIPS,IPART-1,IPART,ISTRS,.FALSE.)
            CALL ARCOLI(IDIPS,-ISTRS)

C...If the end of a string check colour flow and consistency
          ELSEIF (K(I,1).EQ.1) THEN
            CALL ARGTYP(I,ITYP)
            IF (ITYP.EQ.0) CALL ARERRM('ARPARS',1,I)
            IML=I
            IPART=IPART+1
            IDIPS=IDIPS+1
            CALL ARCOPA(I,IPART,ITYP)
            CALL ARCRDI(IDIPS,IPART-1,IPART,ISTRS,.FALSE.)
            CALL ARCOLI(IDIPS,-ISTRS)
C...........If purely gluonic string create extra dipole
            IF (ITYP.EQ.2) THEN
              IF (IDIR.NE.2) CALL ARERRM('ARPARS',4,I)
              IDIPS=IDIPS+1
              CALL ARCRDI(IDIPS,IPART,IPFST,ISTRS,.FALSE.)
              CALL ARCOLI(IDIPS,-ISTRS)
C...........If ordinary string create EM-dipole
            ELSE
              IF (ITYP.NE.-IDIR) CALL ARERRM('ARPARS',5,I)
              IF (MSTA(20).GT.0.AND.IDIPS.EQ.1.AND.
     $               (.NOT.QEX(IPFST)).AND.(.NOT.QEX(IPART))) THEN
                IDIPS=IDIPS+1
                CALL ARCRDI(IDIPS,IPART,IPFST,ISTRS,.TRUE.)
              ENDIF
            ENDIF

C...Initialize string variables in dipole record and perform cascade
            IF (MSTA(14).GE.1) PT2LST=MIN(PT2LST,ARMIPT(IPFST,IPART))
            IF (PARA(6).GT.0.0) PT2LST=MIN(PT2LST,PARA(6))
            IPF(ISTRS)=IPFST
            IPL(ISTRS)=IPART
            IFLOW(ISTRS)=IDIR
            CALL AREXMA(IPFST,IPART)
            IDIR=0
          ENDIF
        ENDIF
 100  CONTINUE

      DO 220 J=1,NR
        IR(J)=-IR(J)
 220  CONTINUE

      RETURN

C**** END OF ARPARS ****************************************************
      END
