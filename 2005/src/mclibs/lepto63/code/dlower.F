
C **********************************************************************

      FUNCTION DLOWER(V1)

C...Lower limit on second variable (y, Q**2 or W**2) depending on first
C...variable x=V1. Used for integrating differential cross-section.

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
C...CMS energy squared and target nucleon mass.
      S=PARL(21)
      PM2=PSAVE(3,2,5)**2
      IF(LST(31).EQ.1) THEN
        DLOWER=MAX(Q2MIN,V1*YMIN*S,(W2MIN-PM2)*V1/MAX(1.-V1,1.E-22))
      ELSEIF(LST(31).EQ.2) THEN
        DLOWER=MAX(YMIN,Q2MIN/(S*V1),(W2MIN-PM2)/MAX(S*(1.-V1),1.E-22))
      ELSEIF(LST(31).EQ.3) THEN
        DLOWER=MAX(W2MIN,(1.-V1)*YMIN*S+PM2,
     &  Q2MIN*(1.-V1)/MAX(V1,1.E-22)+PM2)
      ENDIF
      RETURN
      END
