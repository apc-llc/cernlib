*
* $Id: pystat.F,v 1.1.1.1 1996/01/11 14:05:26 mclareni Exp $
*
* $Log: pystat.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:26  mclareni
* Fritiof
*
*
C***********************************************************************
 
      SUBROUTINE PYSTAT(MSTAT)
 
C...Prints out information about cross-sections, decay widths, branching
C...ratios, kinematical limits, status codes and parameter values.
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/PYINT4/WIDP(21:40,0:40),WIDE(21:40,0:40),WIDS(21:40,3)
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)
      COMMON/PYINT6/PROC(0:200)
      CHARACTER PROC*28
      SAVE /LUDAT1/,/LUDAT2/,/LUDAT3/
      SAVE /PYSUBS/,/PYPARS/,/PYINT1/,/PYINT4/,/PYINT5/,/PYINT6/
      CHARACTER CHAU*16,CHPA(-40:40)*9,CHIN(2)*12,
     &STATE(-1:5)*4,CHKIN(21)*18
      DATA STATE/'----','off ','on  ','on/+','on/-','on/1','on/2'/,
     &CHKIN/' m_hard (GeV/c^2) ',' p_T_hard (GeV/c) ',
     &'m_finite (GeV/c^2)','   y*_subsystem   ','     y*_large     ',
     &'     y*_small     ','    eta*_large    ','    eta*_small    ',
     &'cos(theta*)_large ','cos(theta*)_small ','       x_1        ',
     &'       x_2        ','       x_F        ',' cos(theta_hard)  ',
     &'m''_hard (GeV/c^2) ','       tau        ','        y*        ',
     &'cos(theta_hard^-) ','cos(theta_hard^+) ','      x_T^2       ',
     &'       tau''       '/
 
C...Cross-sections.
      IF(MSTAT.LE.1) THEN
        WRITE(MSTU(11),5000)
        WRITE(MSTU(11),5100)
        WRITE(MSTU(11),5200) 0,PROC(0),NGEN(0,3),NGEN(0,1),XSEC(0,3)
        DO 100 I=1,200
        IF(MSUB(I).NE.1) GOTO 100
        WRITE(MSTU(11),5200) I,PROC(I),NGEN(I,3),NGEN(I,1),XSEC(I,3)
  100   CONTINUE
        WRITE(MSTU(11),5300) 1.-FLOAT(NGEN(0,3))/
     &  MAX(1.,FLOAT(NGEN(0,2)))
 
C...Decay widths and branching ratios.
      ELSEIF(MSTAT.EQ.2) THEN
        DO 110 KF=-40,40
        CALL LUNAME(KF,CHAU)
  110   CHPA(KF)=CHAU(1:9)
        WRITE(MSTU(11),5400)
        WRITE(MSTU(11),5500)
C...Off-shell branchings.
        DO 130 I=1,17
        KC=I
        IF(I.GE.9) KC=I+2
        IF(I.EQ.17) KC=21
        NGP=0
        IF(KC.LE.20) NGP=(MOD(KC,10)+1)/2
        IF(NGP.LE.MSTP(1)) WRITE(MSTU(11),5600) KC,CHPA(KC),PMAS(KC,1),
     &  0.,0.,STATE(MDCY(KC,1)),0.
        DO 120 J=1,MDCY(KC,3)
        IDC=J+MDCY(KC,2)-1
        NGP1=0
        IF(IABS(KFDP(IDC,1)).LE.20) NGP1=
     &  (MOD(IABS(KFDP(IDC,1)),10)+1)/2
        NGP2=0
        IF(IABS(KFDP(IDC,2)).LE.20) NGP2=
     &  (MOD(IABS(KFDP(IDC,2)),10)+1)/2
  120   IF(MDME(IDC,2).EQ.102.AND.NGP1.LE.MSTP(1).AND.NGP2.LE.MSTP(1))
     &  WRITE(MSTU(11),5700) IDC,CHPA(KFDP(IDC,1)),CHPA(KFDP(IDC,2)),
     &  0.,0.,STATE(MDME(IDC,1)),0.
  130   CONTINUE
C...On-shell decays.
        DO 150 I=1,10
        KC=I+22
        IF(I.EQ.4) KC=32
        IF(I.GE.5.AND.I.LE.8) KC=I+29
        IF(I.GE.9) KC=I+30
        IF(WIDE(KC,0).GT.0.) THEN
          WRITE(MSTU(11),5600) KC,CHPA(KC),PMAS(KC,1),WIDP(KC,0),1.,
     &    STATE(MDCY(KC,1)),1.
          DO 140 J=1,MDCY(KC,3)
          IDC=J+MDCY(KC,2)-1
          NGP1=0
          IF(IABS(KFDP(IDC,1)).LE.20) NGP1=
     &    (MOD(IABS(KFDP(IDC,1)),10)+1)/2
          NGP2=0
          IF(IABS(KFDP(IDC,2)).LE.20) NGP2=
     &    (MOD(IABS(KFDP(IDC,2)),10)+1)/2
  140     IF(NGP1.LE.MSTP(1).AND.NGP2.LE.MSTP(1)) WRITE(MSTU(11),5700)
     &    IDC,CHPA(KFDP(IDC,1)),CHPA(KFDP(IDC,2)),WIDP(KC,J),
     &    WIDP(KC,J)/WIDP(KC,0),STATE(MDME(IDC,1)),
     &    WIDE(KC,J)/WIDE(KC,0)
        ELSE
          WRITE(MSTU(11),5600) KC,CHPA(KC),PMAS(KC,1),WIDP(KC,0),1.,
     &    STATE(MDCY(KC,1)),0.
        ENDIF
  150   CONTINUE
        WRITE(MSTU(11),5800)
 
C...Allowed incoming partons/particles at hard interaction.
      ELSEIF(MSTAT.EQ.3) THEN
        WRITE(MSTU(11),5900)
        CALL LUNAME(MINT(11),CHAU)
        CHIN(1)=CHAU(1:12)
        CALL LUNAME(MINT(12),CHAU)
        CHIN(2)=CHAU(1:12)
        WRITE(MSTU(11),6000) CHIN(1),CHIN(2)
        DO 160 KF=-40,40
        CALL LUNAME(KF,CHAU)
  160   CHPA(KF)=CHAU(1:9)
        DO 170 I=-20,22
        IF(I.EQ.0) GOTO 170
        IA=IABS(I)
        IF(IA.GT.MSTP(54).AND.IA.LE.10) GOTO 170
        IF(IA.GT.10+2*MSTP(1).AND.IA.LE.20) GOTO 170
        WRITE(MSTU(11),6100) CHPA(I),STATE(KFIN(1,I)),CHPA(I),
     &  STATE(KFIN(2,I))
  170   CONTINUE
        WRITE(MSTU(11),6200)
 
C...User-defined limits on kinematical variables.
      ELSEIF(MSTAT.EQ.4) THEN
        WRITE(MSTU(11),6300)
        WRITE(MSTU(11),6400)
        SHRMAX=CKIN(2)
        IF(SHRMAX.LT.0.) SHRMAX=VINT(1)
        WRITE(MSTU(11),6500) CKIN(1),CHKIN(1),SHRMAX
        PTHMIN=MAX(CKIN(3),CKIN(5))
        PTHMAX=CKIN(4)
        IF(PTHMAX.LT.0.) PTHMAX=0.5*SHRMAX
        WRITE(MSTU(11),6600) CKIN(3),PTHMIN,CHKIN(2),PTHMAX
        WRITE(MSTU(11),6700) CHKIN(3),CKIN(6)
        DO 180 I=4,14
  180   WRITE(MSTU(11),6500) CKIN(2*I-1),CHKIN(I),CKIN(2*I)
        SPRMAX=CKIN(32)
        IF(SPRMAX.LT.0.) SPRMAX=VINT(1)
        WRITE(MSTU(11),6500) CKIN(31),CHKIN(15),SPRMAX
        WRITE(MSTU(11),6800)
 
C...Status codes and parameter values.
      ELSEIF(MSTAT.EQ.5) THEN
        WRITE(MSTU(11),6900)
        WRITE(MSTU(11),7000)
        DO 190 I=1,100
  190   WRITE(MSTU(11),7100) I,MSTP(I),PARP(I),100+I,MSTP(100+I),
     &  PARP(100+I)
      ENDIF
 
C...Formats for printouts.
 5000 FORMAT('1',9('*'),1X,'PYSTAT:  Statistics on Number of ',
     &'Events and Cross-sections',1X,9('*'))
 5100 FORMAT(/1X,78('=')/1X,'I',34X,'I',28X,'I',12X,'I'/1X,'I',12X,
     &'Subprocess',12X,'I',6X,'Number of points',6X,'I',4X,'Sigma',3X,
     &'I'/1X,'I',34X,'I',28X,'I',12X,'I'/1X,'I',34('-'),'I',28('-'),
     &'I',4X,'(mb)',4X,'I'/1X,'I',34X,'I',28X,'I',12X,'I'/1X,'I',1X,
     &'N:o',1X,'Type',25X,'I',4X,'Generated',9X,'Tried',1X,'I',12X,
     &'I'/1X,'I',34X,'I',28X,'I',12X,'I'/1X,78('=')/1X,'I',34X,'I',28X,
     &'I',12X,'I')
 5200 FORMAT(1X,'I',1X,I3,1X,A28,1X,'I',1X,I12,1X,I13,1X,'I',1X,1P,
     &E10.3,1X,'I')
 5300 FORMAT(1X,'I',34X,'I',28X,'I',12X,'I'/1X,78('=')//
     &1X,'********* Fraction of events that fail fragmentation ',
     &'cuts =',1X,F8.5,' *********'/)
 5400 FORMAT('1',17('*'),1X,'PYSTAT:  Decay Widths and Branching ',
     &'Ratios',1X,17('*'))
 5500 FORMAT(/1X,78('=')/1X,'I',29X,'I',13X,'I',12X,'I',6X,'I',12X,'I'/
     &1X,'I',1X,'Branching/Decay Channel',5X,'I',1X,'Width (GeV)',1X,
     &'I',7X,'B.R.',1X,'I',1X,'Stat',1X,'I',2X,'Eff. B.R.',1X,'I'/1X,
     &'I',29X,'I',13X,'I',12X,'I',6X,'I',12X,'I'/1X,78('='))
 5600 FORMAT(1X,'I',29X,'I',13X,'I',12X,'I',6X,'I',12X,'I'/1X,'I',1X,
     &I4,1X,A9,'(',1P,E8.2,0P,')',1X,'->',1X,'I',2X,1P,E10.3,0P,1X,
     &'I',1X,1P,E10.3,0P,1X,'I',1X,A4,1X,'I',1X,1P,E10.3,0P,1X,'I')
 5700 FORMAT(1X,'I',1X,I4,1X,A9,1X,'+',1X,A9,2X,'I',2X,1P,E10.3,0P,
     &1X,'I',1X,1P,E10.3,0P,1X,'I',1X,A4,1X,'I',1X,1P,E10.3,0P,1X,'I')
 5800 FORMAT(1X,'I',29X,'I',13X,'I',12X,'I',6X,'I',12X,'I'/1X,78('='))
 5900 FORMAT('1',7('*'),1X,'PYSTAT: Allowed Incoming Partons/',
     &'Particles at Hard Interaction',1X,7('*'))
 6000 FORMAT(/1X,78('=')/1X,'I',38X,'I',37X,'I'/1X,'I',1X,
     &'Beam particle:',1X,A12,10X,'I',1X,'Target particle:',1X,A12,7X,
     &'I'/1X,'I',38X,'I',37X,'I'/1X,'I',1X,'Content',6X,'State',19X,
     &'I',1X,'Content',6X,'State',18X,'I'/1X,'I',38X,'I',37X,'I'/1X,
     &78('=')/1X,'I',38X,'I',37X,'I')
 6100 FORMAT(1X,'I',1X,A9,5X,A4,19X,'I',1X,A9,5X,A4,18X,'I')
 6200 FORMAT(1X,'I',38X,'I',37X,'I'/1X,78('='))
 6300 FORMAT('1',12('*'),1X,'PYSTAT: User-Defined Limits on ',
     &'Kinematical Variables',1X,12('*'))
 6400 FORMAT(/1X,78('=')/1X,'I',76X,'I')
 6500 FORMAT(1X,'I',16X,1P,E10.3,0P,1X,'<',1X,A,1X,'<',1X,1P,E10.3,0P,
     &16X,'I')
 6600 FORMAT(1X,'I',3X,1P,E10.3,0P,1X,'(',1P,E10.3,0P,')',1X,'<',1X,A,
     &1X,'<',1X,1P,E10.3,0P,16X,'I')
 6700 FORMAT(1X,'I',29X,A,1X,'=',1X,1P,E10.3,0P,16X,'I')
 6800 FORMAT(1X,'I',76X,'I'/1X,78('='))
 6900 FORMAT('1',12('*'),1X,'PYSTAT: Summary of Status Codes and ',
     &'Parameter Values',1X,12('*'))
 7000 FORMAT(/3X,'I',4X,'MSTP(I)',9X,'PARP(I)',20X,'I',4X,'MSTP(I)',9X,
     &'PARP(I)'/)
 7100 FORMAT(1X,I3,5X,I6,6X,1P,E10.3,0P,18X,I3,5X,I6,6X,1P,E10.3)
 
      RETURN
      END
