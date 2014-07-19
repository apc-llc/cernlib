*
* $Id: bradlp.F,v 1.1.1.1 1996/01/11 14:14:33 mclareni Exp $
*
* $Log: bradlp.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:33  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BRADLP
C     *****************
C--  HANDLES LEPTONIC DECAY MODES OF WEAK BOSON INCLUDING QED
C--  RADIATION
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/berend.inc"
#include "cojets/boflag.inc"
#include "cojets/boson.inc"
#include "cojets/ctopdc.inc"
#include "cojets/data1.inc"
#include "cojets/data2.inc"
#include "cojets/decpar.inc"
#include "cojets/edpar.inc"
#include "cojets/event.inc"
#include "cojets/isjetn.inc"
#include "cojets/itapes.inc"
#include "cojets/jet.inc"
#include "cojets/jetnpc.inc"
#include "cojets/parqua.inc"
#include "cojets/photbe.inc"
#include "cojets/qcds.inc"
#include "cojets/radlep.inc"
#include "cojets/zpar2.inc"
#include "cojets/zwpar.inc"
#include "cojets/weakon.inc"
      DIMENSION QP(4),QM(4),QK(4),QE(4),QN(4)
      DIMENSION COEF(2,2),QQV(2)
C
      DATA ICALL/0/
C
      IF(ICALL.GT.0) GO TO 10
      ICALL=1
      DO 1 J1=1,2
      DO 1 J2=1,2
      FSIGN=1.
      IF(J2.EQ.1) FSIGN=-1.
1     COEF(J1,J2)=(AL(1)**2+BL(1)**2)*(AQ(J1)**2+BQ(J1)**2)
     1           +FSIGN*4.*AL(1)*BL(1)*AQ(J1)*BQ(J1)
      ALFA=ALFQED
      BEALFA=ALFA
      BEXGZ=1.E-6
      BEXGWT=1.E-6
      QQV(1)=2./3.
      QQV(2)=-1./3.
10    CONTINUE
C
C-- Q.N.
      IFBOS=PBOS(6)
      IDC=ICHDB
      KL1=KDP(IDC,1)
      KL2=KDP(IDC,2)
      IF(IFBOS.NE.4) GO TO 12
      ID1=IDENTF(KL1)
      ID2=IDENTF(KL2)
      KL1=INTID(-ID1)
      KL2=INTID(-ID2)
12    CONTINUE
C-- KINEMATICS
      W=PBOS(5)
      S=W**2
      AM1=PMAS(KL1)
      AM2=PMAS(KL2)
      WL=MAX(AM1,AM2)
      IF(IFBOS.NE.1) THEN
        ICHN=ICHDB-IDB(IFBOS)+1
      ELSE
        ICHN=ICHDB
      ENDIF
C
      IF(IRADLP.EQ.2.AND.IFBOS.NE.1) GO TO 200
C
C--  MULTIPHOTON RADIATION (LPA CORRECTED)
C    =====================================
      IF(IFBOS.EQ.3.OR.IFBOS.EQ.4) GO TO 130
      IF(IFBOS.EQ.1.AND.MOPTWZ.EQ.0) GO TO 160
C
C--  Z0 DECAY
C    --------
      CALL Z0RAD(W,WL,NP)
      IF(NP.EQ.2) GO TO 120
      PP2F=P(1,1)**2+P(1,2)**2+P(1,3)**2
      PP2L=P(NP,1)**2+P(NP,2)**2+P(NP,3)**2
      PPF=SQRT(PP2F)
      PPL=SQRT(PP2L)
      LQ=1
      IF(3.*CHARGE(ABS(IQRKFW)).LT.1.5) LQ=2
C-- NOTE: 1ST ENTRY IN ARRAY FROM Z0RAD IS LEPTON ALIGNED ALONG -Z.
C-- LAST ENTRY IS THE OTHER LEPTON.
C-- INV=1 --> NEGATIVE LEPTON FIRST IN ARRAY
      INV=1
      IF(CJRN(1).GT..5) INV=-1
      IF(MOPTWZ.EQ.0) THEN
      IF(IQRKFW.LT.0) GO TO 101
      C1=COEF(LQ,1)
      C2=COEF(LQ,2)
      GO TO 102
101   C1=COEF(LQ,2)
      C2=COEF(LQ,1)
102   CONTINUE
      ELSE
        LXF=1
        FS1=SX*DS*DEN
        FS2=SX**2*DEN
        ABW1=SZF(1,LXI,LXF)
     *      +SZF(2,LXI,LXF)*FS1
     *      +SZF(5,LXI,LXF)*FS2
        ABW2=(SZF(7,LXI,LXF)*FS1+SZF(8,LXI,LXF)*FS2)*.5
        FSIGN=1.
        IF(IQRKFW.LT.0) FSIGN=-1.
        C1=ABW1-FSIGN*ABW2
        C2=ABW1+FSIGN*ABW2
      ENDIF
      WGX=4.*(PP2F+PP2L)*(C1+C2)
C
103   CALL GROT(NP,PHIG,THETA,PF3,PL3,CG,SG)
      IF(INV.LT.0) GO TO 104
      WG=C1*((PPF-PF3)**2+(PPL+PL3)**2)
     1  +C2*((PPF+PF3)**2+(PPL-PL3)**2)
      GO TO 105
104   WG=C1*((PPL-PL3)**2+(PPF+PF3)**2)
     1  +C2*((PPL+PL3)**2+(PPF-PF3)**2)
105   IF(CJRN(1).GT.WG/WGX) GO TO 103
C
      IF(INV.LT.0) GO TO 106
      K(1,2)=KL1
      K(NP,2)=KL2
      GO TO 107
106   K(NP,2)=KL1
      K(1,2)=KL2
107   NPM=NP-1
      DO 108 I=2,NPM
108   K(I,2)=1
      DO 109 I=1,NP
      P1=P(I,1)
      P2=P(I,2)
      P(I,1)= P1*CG+P2*SG
109   P(I,2)=-P1*SG+P2*CG
      GO TO 110
C--  NO PHOTON EMISSION
120   K(1,2)=KL2
      K(2,2)=KL1
      INV=-1
      CALL ZDANG(THETA)
110   GO TO 401
C
130   CONTINUE
C
C--  W DECAY
C    -------
      CALL WRAD(W,WL,NP)
      IF(NP.EQ.2) GO TO 150
      INV=1
      K(1,2)=KL1
      K(NP,2)=KL2
135   NPM=NP-1
      DO 136 I=2,NPM
136   K(I,2)=1
      IF(IBOFLA.NE.0) GO TO 134
C
      PP2N=P(1,1)**2+P(1,2)**2+P(1,3)**2
      PP2C=P(NP,1)**2+P(NP,2)**2+P(NP,3)**2
      PPN=SQRT(PP2N)
      PPC=SQRT(PP2C)
      WGX=4.*(PP2N+PP2C)
      F=1.
      IF(IQRKFW.LT.0) F=-F
      IF(IFBOS.EQ.4) F=-F
133   CALL GROT(NP,PHIG,THETA,PN3,PC3,CG,SG)
      WG=(PPN+F*PN3)**2+(PPC-F*PC3)**2
      IF(CJRN(1).GT.WG/WGX) GO TO 133
C
      DO 139 I=1,NP
      P1=P(I,1)
      P2=P(I,2)
      P(I,1)= P1*CG+P2*SG
139   P(I,2)=-P1*SG+P2*CG
      GO TO 140
C--  NO PHOTON EMISSION
150   CONTINUE
      K(1,2)=KL1
      K(2,2)=KL2
134   CALL WDANG(THETA)
C-- REFLECTION BECAUSE WRAD DIRECTS 1ST DECAY PARTICLE (NU) ALONG -Z
      THETA=PI-THETA
      INV=1
140   GO TO 401
C
C- CONTINUUM DRELL-YAN OPTION 0
C- ----------------------------
160   CONTINUE
      CALL Z0RAD(W,WL,NP)
      IF(NP.EQ.2) GO TO 180
      PP2F=P(1,1)**2+P(1,2)**2+P(1,3)**2
      PP2L=P(NP,1)**2+P(NP,2)**2+P(NP,3)**2
      PPF=SQRT(PP2F)
      PPL=SQRT(PP2L)
      WGX=2.*(PP2F+PP2L)
C-- INV=1 --> NEGATIVE LEPTON FIRST IN ARRAY
      INV=1
      IF(CJRN(1).GT..5) INV=-1
163   CALL GROT(NP,PHIG,THETA,PF3,PL3,CG,SG)
      WG=PP2F+PF3**2+PP2L+PL3**2
      IF(CJRN(1).GT.WG/WGX) GO TO 163
C
      IF(INV.LT.0) GO TO 166
      K(1,2)=KL1
      K(NP,2)=KL2
      GO TO 167
166   K(NP,2)=KL1
      K(1,2)=KL2
167   NPM=NP-1
      DO 168 I=2,NPM
168   K(I,2)=1
      DO 169 I=1,NP
      P1=P(I,1)
      P2=P(I,2)
      P(I,1)= P1*CG+P2*SG
169   P(I,2)=-P1*SG+P2*CG
      GO TO 170
C--  NO PHOTON EMISSION
180   K(1,2)=KL1
      K(2,2)=KL2
      INV=1
      CALL DDANG(THETA)
170   GO TO 401
C
C--  FINAL ROTATION AND BOOKEEPING FOR IRADLP=1
401   CONTINUE
C--  EXTRA PARTICLE INFO
      IF(NPART+NP.GT.MXPART) GO TO 500
      IF(IBOFLA.NE.0) GO TO 404
      DO 402 I=1,NP
      IP=I+NPART
      COSG=(P(1,1)-P(NP,1))*P(I,1)+(P(1,2)-P(NP,2))*P(I,2)
     1    +(P(1,3)-P(NP,3))*P(I,3)
      IORIG(IP)=IPACK*1
      IF(FLOAT(INV)*COSG.LT.0.) IORIG(IP)=IPACK*2
      IDENT(IP)=IDENTF(K(I,2))
      IDCAY(IP)=0
402   CONTINUE
404   CONTINUE
C
C--  FINAL ROTATION OF ARRAY
      PHI=PI2*CJRN(1)
      CALL EDITP(NP)
C--  EVINT INFORMATION
      IF(IBOFLA.EQ.0) CALL BEVINT(KL1,KL2,NP,INV)
      GO TO 400
C
C--  O(ALPHA) EMISSION ACCORDING TO BERENDS ET AL.
C    =============================================
200   CONTINUE
      IF(IFBOS.GT.2) GO TO 230
C
C--  Z0 DECAY
C    --------
      BECVL=AL(1)
      BECAL=BL(1)
      LQ=1
      IF(3.*CHARGE(ABS(IQRKFW)).LT.1.5) LQ=2
      BEQQ=QQV(LQ)
      BECVQ=AQ(LQ)
      BECAQ=BQ(LQ)
      BES=S
      BEXM=WL
      BEXMZ=W
      CALL ZDECIF(QP,QM,QK)
      NP=3
      IF(QK(4).EQ.0.0) NP=2
      IF(IQRKFW.LT.0) GO TO 201
      QP(2)=-QP(2)
      QP(3)=-QP(3)
      QM(2)=-QM(2)
      QM(3)=-QM(3)
      QK(2)=-QK(2)
      QK(3)=-QK(3)
201   CONTINUE
      K(1,2)=KL1
      K(NP,2)=KL2
      P(1,5)=WL
      P(NP,5)=WL
      DO 202 J=1,4
      P(1,J)=QM(J)
202   P(NP,J)=QP(J)
      IF(NP.EQ.2) GO TO 203
      K(2,2)=1
      P(2,5)=0.
      DO 204 J=1,4
204   P(2,J)=QK(J)
203   CONTINUE
      GO TO 410
C
C--  W DECAY
C    -------
230   CONTINUE
      BES=S
      BEXMW=W
      BEXM=WL
      CALL WDECIF(QE,QN,QK)
      NP=3
      IF(QK(4).EQ.0.0) NP=2
      IF(IQRKFW.GT.0) GO TO 231
      QE(2)=-QE(2)
      QE(3)=-QE(3)
      QN(2)=-QN(2)
      QN(3)=-QN(3)
      QK(2)=-QK(2)
      QK(3)=-QK(3)
231   CONTINUE
      K(1,2)=KL1
      K(NP,2)=KL2
      P(1,5)=0.
      P(NP,5)=WL
      P(1,4)=QN(4)
      P(NP,4)=QE(4)
      FCH=1.
      IF(IFBOS.EQ.3) FCH=-1.
      DO 233 J=1,3
      P(1,J)=FCH*QN(J)
233   P(NP,J)=FCH*QE(J)
      IF(NP.EQ.2) GO TO 236
      K(2,2)=1.
      P(2,5)=0.
      P(2,4)=QK(4)
      DO 237 J=1,3
237   P(2,J)=-P(1,J)-P(3,J)
236   CONTINUE
C--  BOOKEEPING FOR IRADLP=2
410   CONTINUE
C--  EVINT INFORMATION
      IF(IBOFLA.EQ.0) CALL BEVBER(KL1,KL2,NP,IFBOS)
C--  EXTRA PARTICLE INFO
      IF(NPART+3.GT.MXPART) GO TO 500
      IF(IBOFLA.NE.0) GO TO 400
      IF(NP.EQ.3) GO TO 411
      DO 412 I=1,2
      IP=NPART+I
      IORIG(IP)=IPACK*I
      IDENT(IP)=IDENTF(K(I,2))
      IDCAY(IP)=0
412   CONTINUE
      GO TO 400
411   CONTINUE
      DO 413 I=1,3
      IP=NPART+I
      IDENT(IP)=IDENTF(K(I,2))
413   IDCAY(IP)=0
      IORIG(NPART+1)=IPACK*1
      IORIG(NPART+3)=IPACK*2
      IF(IFBOS.EQ.3.OR.IFBOS.EQ.4) GO TO 414
      COSG=(P(1,1)-P(3,1))*P(2,1)+(P(1,2)-P(3,2))*P(2,2)
     1    +(P(1,3)-P(3,3))*P(2,3)
      IF((IFBOS.EQ.2.OR.IFBOS.EQ.1).AND.COSG.LT.0.) GO TO 414
      IORIG(NPART+2)=IPACK*1
      GO TO 415
414   CONTINUE
      IORIG(NPART+2)=IPACK*2
415   CONTINUE
C
400   CONTINUE
      DO 460 I=1,NP
      KDEC(I,1)=1
      KDEC(I,2)=0
460   K(I,1)=0
      NPRIMR=NP
      IF(ICHN.EQ.3) GO TO 450
C--  DECAY PRODUCTS DO NOT INCLUDE TAU LEPTONS
      RETURN
C
C--  TAU DECAYS
450   CONTINUE
      I=NP
      IPD=0
453   IPD=IPD+1
      KDEC(IPD,1)=I+1
      IF(IDB(K(IPD,2)).GT.0) CALL DECAY(IPD,I)
      IF(WEIGHT.LT.1.E-30) RETURN
      KDEC(IPD,2)=I
      IF(IPD.LT.I) GO TO 453
      NP=I
      IF(NP.EQ.NPRIMR) RETURN
      IF(NPART+NP.GT.MXPART) GO TO 500
      IF(IBOFLA.NE.0) RETURN
      DO 456 I=1,NP
      IP=NPART+I
      IF(I.LE.NPRIMR) GO TO 457
      IORIG(IP)=IPACK*(ABS(IORIG(-K(I,1)+NPART))/IPACK)
     *+(-K(I,1)+NPART)
      IDENT(IP)=IDENTF(K(I,2))
457   CONTINUE
      IDCAY(IP)=0
      IF(KDEC(I,2).GE.KDEC(I,1))
     *IDCAY(IP)=IPACK*KDEC(I,1)+KDEC(I,2)
456   CONTINUE
      RETURN
C
C-- ABNORMAL EXIT
500   WRITE(ITLIS,501) MXPART,NEVENT,NQUA
501   FORMAT(5(/),1X,28HNUMBER OF PARTICLES EXCEEDS   ,I10
     1 //1X,11HEVENT NO. =  ,I10 ,10X,10HNO. JETS =     ,I10
     3//1X,'INCREASE MXPART'
     4 //1X,'EXECUTION TERMINATED'
     4)
      CALL OVERDM
      RETURN
C
      END
