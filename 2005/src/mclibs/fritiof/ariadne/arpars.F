*
* $Id: arpars.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: arpars.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arpars.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      SUBROUTINE ARPARS(NSTART,NEND)
 
C...ARiadne subroutine PARSe the event record
 
C...Parse through the /LUJETS/ event record to find un-cascaded
C...strings. Performs dipole cascade on each found.
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),IEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),IPART
      SAVE /ARPART/
 
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),IDIPS
      SAVE /ARDIPS/
 
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
      COMMON /ARJETX/ N,K(300,5),P(300,5),V(300,5)
      SAVE /ARJETX/
 
 
      IDIR=0
 
C...Loop over entries in /LUJETS/ to be considered
      DO 100 I=NSTART,NEND
 
C...If IDIR=0 there is no current string so skip all entries which
C...are not the begining of a string (K(I,1)=2) otherwise copy
C...parton to dipole record
        IF(IDIR.EQ.0) THEN
          IF(K(I,1).NE.2) GOTO 100
          CALL ARGTYP(I,ITYP)
          IF(ITYP.EQ.0) CALL ARERRM('ARPARS',1,I)
          IDIR=ITYP
          IMF=I
          IPART=1
          IDIPS=0
          CALL ARCOPA(I,IPART,ITYP)
        ELSE
 
C...If in a string, copy parton and create a dipole. Error if
C...colour singlets of triplets are found
          IF(K(I,1).EQ.2) THEN
            CALL ARGTYP(I,ITYP)
            IF(ABS(ITYP).EQ.1) CALL ARERRM('ARPARS',2,I)
            IF(ABS(ITYP).EQ.0) CALL ARERRM('ARPARS',1,I)
            IPART=IPART+1
            IDIPS=IDIPS+1
            CALL ARCOPA(I,IPART,ITYP)
            CALL ARCRDI(IDIPS,IPART-1,IPART,1,.FALSE.)
 
C...If the end of a string check colour flow and consistency
          ELSEIF(K(I,1).EQ.1) THEN
            CALL ARGTYP(I,ITYP)
            IF(ITYP.EQ.0) CALL ARERRM('ARPARS',1,I)
            IML=I
            IPART=IPART+1
            IDIPS=IDIPS+1
            CALL ARCOPA(I,IPART,ITYP)
            CALL ARCRDI(IDIPS,IPART-1,IPART,1,.FALSE.)
C...........If purely gluonic string create extra dipole
            IF(ITYP.EQ.2) THEN
              IF(IDIR.NE.2) CALL ARERRM('ARPARS',4,I)
              IDIPS=IDIPS+1
              CALL ARCRDI(IDIPS,IPART,1,1,.FALSE.)
C...........If ordinary string create EM-dipole
            ELSE
              IF(ITYP.NE.-IDIR) CALL ARERRM('ARPARS',5,I)
              IF(MSTA(20).GT.0.AND.IDIPS.EQ.1.AND.
     $               IEX(1).EQ.0.AND.IEX(IPART).EQ.0) THEN
                IDIPS=IDIPS+1
                CALL ARCRDI(IDIPS,IPART,1,1,.TRUE.)
              ENDIF
            ENDIF
 
C...Initialize string variables in dipole record and perform cascade
            PT2LST=PARA(40)
            IF(MSTA(14).GT.1.AND.IPART.GT.2) PT2LST=ARMIPT(1,IPART)
            IF(PARA(6).GT.0.0) PT2LST=MIN(PT2LST,PARA(6)**2)
C...Don't cascade purelu gluonic string
            IF(IDIR.NE.2) THEN
              IPF(1)=1
              IPL(1)=IPART
              ISTRS=1
              IFLOW(1)=IDIR
              CALL AREXMA(1,IPART)
              CALL ARCASC
              IDIR=0
            ENDIF
          ENDIF
        ENDIF
 100  CONTINUE
 
 
      RETURN
 
C**** END OF ARPARS ****************************************************
      END
