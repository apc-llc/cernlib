*
* $Id: arpars.F,v 1.1.1.1 1996/03/08 16:51:03 mclareni Exp $
*
* $Log: arpars.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:03  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arpars.F,v 1.1.1.1 1996/03/08 16:51:03 mclareni Exp $
 
      SUBROUTINE ARPARS(NSTART,NEND)
 
C...ARiadne subroutine PARSe the event record
 
C...Parse through the /LUJETS/ event record to find un-cascaded
C...strings. Performs dipole cascade on each found.
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),
     $                ICOLI(MAXDIP),IDIPS
      SAVE /ARDIPS/
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,PT2MAX,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
 
 
      IDIR=0
      QQ(MAXPAR-3)=.FALSE.
      QQ(MAXPAR-4)=.FALSE.
 
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
            GOTO 100
          ENDIF
          IF (MHAR(133).GE.3.AND.K(I,3).EQ.0) THEN
            K(I,4)=MAX(K(I,4),0)
            GOTO 100
          ENDIF
          IF (ITYP.EQ.0) CALL ARERRM('ARPARS',1,I)
          IDIR=ITYP
          IMF=I
          IPART=1
          IDIPS=0
          CALL ARCOPA(I,IPART,ITYP)
        ELSE
 
C...If in a string, copy parton and create a dipole. Error if
C...colour singlets of triplets are found
          IF (K(I,1).EQ.2) THEN
            CALL ARGTYP(I,ITYP)
            IF (ABS(ITYP).EQ.1) CALL ARERRM('ARPARS',2,I)
            IF (ABS(ITYP).EQ.0) CALL ARERRM('ARPARS',1,I)
            IPART=IPART+1
            IDIPS=IDIPS+1
            CALL ARCOPA(I,IPART,ITYP)
            CALL ARCRDI(IDIPS,IPART-1,IPART,1,.FALSE.)
            CALL ARCOLI(IDIPS,-1)
 
C...If the end of a string check colour flow and consistency
          ELSEIF (K(I,1).EQ.1) THEN
            CALL ARGTYP(I,ITYP)
            IF (ITYP.EQ.0) CALL ARERRM('ARPARS',1,I)
            IML=I
            IPART=IPART+1
            IDIPS=IDIPS+1
            CALL ARCOPA(I,IPART,ITYP)
            CALL ARCRDI(IDIPS,IPART-1,IPART,1,.FALSE.)
            CALL ARCOLI(IDIPS,-1)
C...........If purely gluonic string create extra dipole
            IF (ITYP.EQ.2) THEN
              IF (IDIR.NE.2) CALL ARERRM('ARPARS',4,I)
              IDIPS=IDIPS+1
              CALL ARCRDI(IDIPS,IPART,1,1,.FALSE.)
              CALL ARCOLI(IDIPS,-1)
C...........If ordinary string create EM-dipole
            ELSE
              IF (ITYP.NE.-IDIR) CALL ARERRM('ARPARS',5,I)
              IF (MSTA(20).GT.0.AND.IDIPS.EQ.1.AND.
     $               (.NOT.QEX(1)).AND.(.NOT.QEX(IPART))) THEN
                IDIPS=IDIPS+1
                CALL ARCRDI(IDIPS,IPART,1,1,.TRUE.)
              ENDIF
            ENDIF
 
C...Initialize string variables in dipole record and perform cascade
            PT2LST=PARA(40)
            IF (MSTA(14).GE.1.AND.IPART.GT.2) PT2LST=ARMIPT(1,IPART)
            IF (PARA(6).GT.0.0) PT2LST=MIN(PT2LST,PARA(6))
            IF (MHAR(133).LT.0) PT2LST=P(I,1)**2+P(I,2)**2
C...Special case if purely gluonic string
            IF (IDIR.EQ.2) THEN
C...Don't allow purely gluonic strings
C              IDIR=0
C              GOTO 100
            ENDIF
            IPF(1)=1
            IPL(1)=IPART
            ISTRS=1
            IFLOW(1)=IDIR
            CALL AREXMA(1,IPART)
            QDUMP=.FALSE.
            CALL ARCASC
            IDIR=0
          ENDIF
        ENDIF
 100  CONTINUE
 
 
      RETURN
 
C**** END OF ARPARS ****************************************************
      END
