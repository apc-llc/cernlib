*
* $Id: igpie.F,v 1.2 2001/05/31 07:33:59 couet Exp $
*
* $Log: igpie.F,v $
* Revision 1.2  2001/05/31 07:33:59  couet
* - The variable TXT was too small (12). Now it is 32.
*
* Revision 1.1.1.1  1996/02/14 13:10:37  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.12/14 25/06/91  14.32.31  by  O.Couet
*-- Author :
      SUBROUTINE IGPIE(X,Y,R,N,V,CHOPT,IAO,IAS,IAC)
*.===========>
*.
*.   To draw a PIE chart
*.
*..==========> (R.Nierhaus)
#include "higz/hiatt.inc"
#include "higz/hilabs.inc"
      DIMENSION V(1),IAO(1),IAS(1),IAC(1)
      CHARACTER*(*) CHOPT
      CHARACTER*12 TEXT
      CHARACTER*32 TXT
      DIMENSION PHI(51),JAS(50),IOPT(8)
      INTEGER SI
      EQUIVALENCE (IOPTO,IOPT(1)),(IOPTS,IOPT(2))
      EQUIVALENCE (IOPTC,IOPT(3)),(IOPTN,IOPT(4))
      EQUIVALENCE (IOPTP,IOPT(5)),(IOPTL,IOPT(6))
      EQUIVALENCE (IOPTH,IOPT(7)),(IOPTR,IOPT(8))
      DATA JAS/111,222,333,444,433,233,211,388,
     +         390,215,225,235,245,265,275,35*550/
*.______________________________________
*
      IF (N.LT.2.OR.N.GT.50) THEN
         CALL IGERR('Wrong number of values','IGPIE')
         RETURN
      ENDIF
      CALL UOPTC(CHOPT,'OSCNPLHR',IOPT)
      IF(IOPTS.EQ.0.AND.IOPTC.EQ.0)IOPTS=2
      IF(IOPTP.NE.0)IOPTN=0
      IF(IOPTL.NE.0)THEN
         IF(NHILAB.LT.N)THEN
            CALL IGERR('Not enough labels defined','IGPIE')
            IOPTL=0
         ENDIF
         IOPTN=0
         IOPTP=0
      ENDIF
      S=0.
      DO 10 I=1,N
         S=S+V(I)
   10 CONTINUE
      F=2.*3.14159/S
      PHI(1)=0.
      DO 70  I=1,N
         PHI(I+1)=PHI(I)+F*V(I)
         PHI1=.5*(PHI(I)+PHI(I+1))
         IF (IOPTS.EQ.1) THEN
            JIABS=IABS(IAS(I))
            IS=JIABS/1000-1
            SI=MOD(JIABS,1000)
            IF(IAS(I).LT.0)SI=-SI
            IF (IS.LT.0) THEN
               CALL ISFAIS(3)
            ELSE
               CALL ISFAIS(IS)
            ENDIF
            IF(SI.NE.0)CALL ISFASI(SI)
         ELSEIF(IOPTS.EQ.2)THEN
            CALL ISFAIS(3)
            CALL ISFASI(JAS(I))
         ENDIF
         IF (IOPTC.NE.0) THEN
            CALL ISFACI(IAC(I))
         ENDIF
         IF (IOPTO.NE.0) THEN
            IF (IAO(I).EQ.0) THEN
               ALPHA=(180./3.14159)*PHI(I)
               BETA =(180./3.14159)*PHI(I+1)
               CALL IGARC(X,Y,0.,R,ALPHA,BETA)
            ELSE
               XX=X+.01*FLOAT(IAO(I))*R*COS(PHI1)
               YY=Y+.01*FLOAT(IAO(I))*R*SIN(PHI1)
               ALPHA=(180./3.14159)*PHI(I)
               BETA =(180./3.14159)*PHI(I+1)
               CALL IGARC(XX,YY,0.,R,ALPHA,BETA)
            ENDIF
         ELSE
            ALPHA=(180./3.14159)*PHI(I)
            BETA =(180./3.14159)*PHI(I+1)
            CALL IGARC(X,Y,0.,R,ALPHA,BETA)
         ENDIF
*
         RR=R
         IF (IOPTO.NE.0) RR=R*(1.+.01*FLOAT(IAO(I)))
         IF(IOPTN.NE.0)THEN
            VAL=V(I)
            WRITE(TEXT,'(G12.6)')VAL
         ELSEIF(IOPTP.NE.0)THEN
            VAL=100.*V(I)/S+.0000001
            WRITE(TEXT,'(F5.1)')VAL
         ELSEIF(IOPTL.NE.0)THEN
            TXT=HILABS(I)
            GO TO 60
         ELSE
            GO TO 70
         ENDIF
         DO 20 J=1,12
            IF(TEXT(J:J).NE.' ')THEN
               I1=J
               GO TO 30
            ENDIF
   20    CONTINUE
         I1=12
   30    CONTINUE
         DO 40 J=12,I1,-1
            IF(TEXT(J:J).EQ.' '.OR.TEXT(J:J).EQ.'0')GO TO 40
            I2=J
            IF(TEXT(J:J).EQ.'.')I2=I2-1
            GO TO 50
   40    CONTINUE
   50    CONTINUE
         IF(I2.LT.I1)I2=I1
         TXT=TEXT(I1:I2)
   60    IF(IOPTH.NE.0)THEN
            H=RCHH
         ELSE
            H=R/15.
            IF(N.GT.10)H=H/2.
         ENDIF
         TS=0.
         CALL IGTEXT(0.,0.,TXT,H,TS,'S')
         IF(IOPTR.NE.0)THEN
            CALL IGSET('TANG',(ALPHA+BETA)/2.)
            CALL IGSET('TXAL',3.)
            XX=X+COS(PHI1)*(1.05*RR+0.5*H)
            YY=Y+SIN(PHI1)*(1.05*RR+0.5*H)
         ELSE
            CALL IGSET('TANG',0.)
            CALL IGSET('TXAL',20.)
            XX=X+COS(PHI1)*(1.05*RR+0.5*TS)
            YY=Y+SIN(PHI1)*(1.05*RR+0.5*H) -0.5*H
         ENDIF
         CALL IGSET('CHHE',H)
         CALL ITX(XX,YY,TXT)
   70 CONTINUE
*
      END
