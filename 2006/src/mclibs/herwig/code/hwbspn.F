CDECK  ID>, HWBSPN.
*CMZ :-        -26/04/91  11.11.54  by  Bryan Webber
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWBSPN
C-----------------------------------------------------------------------
C     Constructs appropriate spin density/decay matrix for parton
C     in hard subprocess, othwise zero. Assignments based upon
C     Comp. Phys. Comm. 58 (1990) 271.
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION C,V12,V23,V13,TR,C1,C2,C3,R1(2),R2(2)
      INTEGER IST
      SAVE R1,R2,V12
      IF (IERROR.NE.0) RETURN
      IST=MOD(ISTHEP(NEVPAR),10)
C Assumed partons processed in the order IST=1,2,3,4
      IF (IPROC.GE.100.AND.IPROC.LE.116) THEN
C  An e+e- ---> qqbar g event
         IF (IDPAR(2).EQ.13) THEN
            RHOPAR(1,2)=GPOLN
            RHOPAR(2,2)=0.
            RETURN
         ENDIF
      ELSEIF (IPRO.EQ.15.OR.IPRO.EQ.17) THEN
         IF (IHPRO.EQ. 7.OR.IHPRO.EQ. 8.OR.
     &       IHPRO.EQ.10.OR.IHPRO.EQ.11.OR.
     &       IHPRO.EQ.15.OR.IHPRO.EQ.16.OR.
     &      (IHPRO.GE.21.AND.IHPRO.LE.31)) THEN
C A hard 2 --- > 2 QCD subprocess involving gluons
            IF (IST.EQ.2) THEN
               CALL HWVEQU(2,RHOPAR(1,2),R1(1))
               C=GCOEF(2)/GCOEF(1)
               DECPAR(1,2)=C*R1(1)
               DECPAR(2,2)=C*R1(2)
               RETURN
            ELSEIF (IST.EQ.3) THEN
               CALL HWVEQU(2,RHOPAR(1,2),R2(1))
               V12=R1(1)*R2(1)+R1(2)*R2(2)
               TR=1./(GCOEF(1)+GCOEF(2)*V12)
               RHOPAR(1,2)= (GCOEF(3)*R1(1)+GCOEF(4)*R2(1))*TR
               RHOPAR(2,2)=-(GCOEF(3)*R1(2)+GCOEF(4)*R2(2))*TR
               RETURN
            ELSEIF (IST.EQ.4) THEN
               V13=R1(1)*DECPAR(1,2)+R1(2)*DECPAR(2,2)
               V23=R2(1)*DECPAR(1,2)+R2(2)*DECPAR(2,2)
               TR=1./(GCOEF(1)+GCOEF(2)*V12+GCOEF(3)*V13+GCOEF(4)*V23)
               C1=(GCOEF(2)+GCOEF(5))*TR
               C2=(GCOEF(3)+GCOEF(6))*TR
               C3=(GCOEF(4)+GCOEF(6))*TR
               RHOPAR(1,2)=C1*DECPAR(1,2)+C2*R2(1)+C3*R1(1)
               RHOPAR(2,2)=C1*DECPAR(2,2)-C2*R1(2)-C3*R2(2)
               RETURN
            ENDIF
         ENDIF
      ELSEIF (IPRO.EQ.16) THEN
C A gluon fusion ---> Higgs event
         IF (IST.EQ.2) THEN
            DECPAR(1,2)=RHOPAR(1,2)
            DECPAR(2,2)=-RHOPAR(2,2)
            RETURN
         ENDIF
      ENDIF
      CALL HWVZRO(2,RHOPAR(1,2))
      CALL HWVZRO(2,DECPAR(1,2))
      END
