#include "isajet/pilot.h"
      INTEGER FUNCTION ITRANS(ID,MCONV)
C
C          Convert (MCONV=1) from ISAJET numbering to PDG numbering
C               or (MCONV=2) from PDG numbering to ISAJET numbering
C
C          --- Begin Table of ISAJET/PDG particle codes ---
C
C          NAME      ISAJET    PDG
C          UP        1         2
C          DN        2         1
C          ST        3         3
C          CH        4         4
C          BT        5         5
C          TP        6         6
C          Y         7         7
C          X         8         8
C          GL        9         21
C          GM        10        22
C
C          NUE       11        12
C          E-        12        11
C          NUMU      13        14
C          MU-       14        13
C          NUTAU     15        14
C          TAU-      16        15
C          KS        20        310
C
C          UPLSS     21        1000002
C          DNLSS     22        1000001
C          STLSS     23        1000003
C          CHLSS     24        1000004
C          BT1SS     25        1000005
C          TP1SS     26        1000006
C          GLSS      29        1000021
C          Z1SS      30        1000022
C
C          NUEL      31        1000012
C          EL-       32        1000011
C          NUML      33        1000014
C          MUL-      34        1000013
C          NUTL      35        1000016
C          TAU1-     36        1000015
C          W1SS+     39        1000024
C          Z2SS      40        1000023
C
C          UPRSS     41        2000002
C          DNRSS     42        2000001
C          STRSS     43        2000003
C          CHRSS     44        2000004
C          BT2SS     45        2000005
C          TP2SS     46        2000006
C          W2SS+     49        1000037
C          Z3SS      50        1000025
C
C          NUER      51        2000012
C          ER-       52        2000011
C          NUMR      53        2000014
C          MUR-      54        2000013
C          NUTR      55        2000016
C          TAU2-     56        2000015
C          Z4SS      60        1000035
C
C          W+        80        24
C          HIGGS     81        25
C          HL0       82        51
C          HH0       83        35
C          HA0       84        36
C          H+        86        37
C          Z0        90        23
C
C          GVSS      91        1000039
C          GRAV      92        39
C
C          --- End Table of ISAJET/PDG particle codes ---
C
C          Ver 7.21: add extra mesons with IABS(ID) > 10000; these only 
C          occur in a few B decays.
C          Ver 7.52: Fix bug in special mesons
C          Ver 7.55: Update for new PDG numbering scheme.
C
C     Thanks to Lynn Garren, Fermilab.
C
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/itapes.inc"
C
      INTEGER ID,MCONV
      INTEGER I2,I3,J,IDA,IF1,IF2,IND,ITMP,IF3,JS,J1,IS1,IS2,IS3,I4,I1
C
C... ITABI(I) converts miscellaneous ISAJET particle ID's to standard 
C... scheme
C
      INTEGER ITABI(99,2), NOANT(15)
      SAVE ITABI,NOANT
C
      DATA ITABI/2,1,3,4,5,6,7,8,21,22,
     1 12,11,14,13,16,15,0,0,0,310,
     2 1000002,1000001,1000003,1000004,1000005,1000006,0,0,1000021,
     2 1000022,
     3 1000012,1000011,1000014,1000013,1000016,1000015,0,0,1000024,
     3 1000023,
     4 2000002,2000001,2000003,2000004,2000005,2000006,0,0,1000037,
     4 1000025,
     5 2000012,2000011,2000014,2000013,2000016,2000015,0,0,0,1000035,
     6 0,0,0,0,0,0,0,0,0,0,             0,0,0,0,0,0,0,0,0,24,    
     8 25,51,35,36,55,37,53,52,54,23,   1000039,39,0,0,0,0,0,0,0,
     * 2,1,3,4,5,6,7,8,0,0,             12,11,14,13,16,15,0,0,0,0, 
     2 9,10,90,80,81,0,0,0,0,0,         0,0,0,0,83,84,86,0,92,0,
     4 0,0,0,0,0,0,0,0,0,0,             82,88,87,89,85,0,0,0,0,0,  
     6 0,0,0,0,0,0,0,0,0,0,             0,0,0,0,0,0,0,0,0,0,
     8 0,0,0,0,0,0,0,0,0,0,             0,0,0,0,0,0,0,0,0/
C
      DATA NOANT/-21,-22,-23,-25,-30,-35,-47,-48,-57,-58,-59,
     1 -67,-68,-69,-70/
C
C          Entry
C
      IDA=IABS(ID)
      ITRANS=0
      IF(MCONV.NE.1) GO TO 200
C
C          Convert ISAJET to PDG
C
      CALL FLAVOR(ID,IF1,IF2,IF3,JS,IND)
      IF(IDA.EQ.0) THEN
        WRITE(ITLIS,*) ' ITRANS: particle ID is zero'
      ELSEIF(IDA.LT.100) THEN
        ITRANS=ISIGN(ITABI(IDA,1),ID)
        IF(ID.EQ.-20) ITRANS=130
C...check for illegal antiparticles
        ITMP=ITRANS
        IF(ITMP.LT.0) THEN
          DO 101 J=1,15
            IF(ITMP.EQ.NOANT(J)) ITRANS=0
 101      CONTINUE
        ENDIF
      ELSEIF(IND.NE.0.AND.IDA.LT.10000) THEN
        IS1=IABS(IF1)
        IS2=IABS(IF2)
        IS3=IABS(IF3)
C...mesons
        IF(IS1.EQ.0) THEN
          IF(IS2.LE.2 .AND. IS3.LE.2) THEN
C...         don't change
          ELSE
C...         u and d have opposite definitions
            IF(IS2.LE.2) IS2=ITABI(IS2,1)
            IF(IS3.LE.2) IS3=ITABI(IS3,1)
          ENDIF
          ITRANS=IS3*100 + IS2*10 + 2*JS+1
          ITRANS=ISIGN(ITRANS,ID)
C...         charmed and top mesons have wrong sign
          IF(IS3.EQ.4 .AND. IS2.NE.4) ITRANS=-ITRANS
          IF(IS3.EQ.6 .AND. IS2.NE.6 .AND. IS2.NE.4) ITRANS=-ITRANS
C...check for illegal antiparticles
          IF(IS2.EQ.IS3 .AND. ID.LT.0) ITRANS=0
C...diquarks
        ELSEIF(IS3.EQ.0) THEN
C...         u and d have opposite definitions
          IF(IS1.LE.2) IS1=ITABI(IS1,1)
          IF(IS2.LE.2) IS2=ITABI(IS2,1)
          IF(IS2.LT.IS1) THEN
            ITRANS=IS1*1000 + IS2*100 + 2*JS+1
          ELSE
            ITRANS=IS2*1000 + IS1*100 + 2*JS+1
          ENDIF
          ITRANS=ISIGN(ITRANS,ID)
C...         charmed and top mesons have wrong sign
          IF(IS2.EQ.4 .AND. IS1.NE.4) ITRANS=-ITRANS
          IF(IS2.EQ.6 .AND. IS1.NE.6 .AND. IS1.NE.4) ITRANS=-ITRANS
C...baryons
        ELSE
C...         u and d have opposite definitions
          IF(IS1.LE.2) IS1=ITABI(IS1,1)
          IF(IS2.LE.2) IS2=ITABI(IS2,1)
          IF(IS3.LE.2) IS3=ITABI(IS3,1)
          IF(IS3.LE.2) THEN
            ITRANS=IS1*1000 + IS2*100 + IS3*10 + 2*JS+2
          ELSEIF(IS1.LE.2 .AND. IS2.LE.2) THEN
            ITRANS=IS3*1000 + IS1*100 + IS2*10 + 2*JS+2
          ELSE
            ITRANS=IS3*1000 + IS2*100 + IS1*10 + 2*JS+2
          ENDIF
          ITRANS=ISIGN(ITRANS,ID)
        ENDIF
      ELSEIF(IND.GT.0.AND.IDA.GT.10000) THEN
C...Special mesons. 
        IF(IDA.EQ.10121) THEN
          ITRANS=ISIGN(20213,ID)
        ELSEIF(IDA.EQ.10111) THEN
          ITRANS=ISIGN(20113,ID)
        ELSEIF(IDA.EQ.10131) THEN
          ITRANS=ISIGN(10323,ID)
        ELSEIF(IDA.EQ.10231) THEN
          ITRANS=ISIGN(10313,ID)
        ELSEIF(IDA.EQ.30131) THEN
          ITRANS=ISIGN(30323,ID)
        ELSEIF(IDA.EQ.30231) THEN
          ITRANS=ISIGN(30313,ID)
        ELSEIF(IDA.EQ.10110) THEN
          ITRANS=ISIGN(10221,ID)
        ELSEIF(IDA.EQ.10441) THEN
          ITRANS=ISIGN(20443,ID)
        ELSEIF(IDA.EQ.20440) THEN
          ITRANS=ISIGN(10441,ID)
        ELSEIF(IDA.EQ.20441) THEN
          ITRANS=ISIGN(10443,ID)
        ELSEIF(IDA.EQ.20442) THEN
          ITRANS=ISIGN(445,ID)
        ELSE
          ITRANS=ID
        ENDIF
      ENDIF
      GO TO 300
C
C          Convert PDG to ISAJET
C
 200  IF(MCONV.NE.2) GO TO 300
      J1=MOD(IDA,10)
      I1=MOD(IDA/10,10)
      I2=MOD(IDA/100,10)
      I3=MOD(IDA/1000,10)
      I4=MOD(IDA/10000,10)
      IF(IDA.EQ.0) THEN
        WRITE(ITLIS,*) ' ITRANS: particle ID is zero'
C...elementary particles
      ELSEIF(IDA.LT.100) THEN
        ITRANS=ISIGN(ITABI(IDA,2),ID)
C...check for illegal antiparticles
        IF(ID.LT.0) THEN
          DO 201 J=1,15
            IF(ID.EQ.NOANT(J)) ITRANS=0
 201      CONTINUE
        ENDIF
C...K short and K long
      ELSEIF(ID.EQ.130) THEN
        ITRANS=-20
      ELSEIF(ID.EQ.310) THEN
        ITRANS=20
C...mesons
      ELSEIF(I3.EQ.0) THEN
          IF(I1.LE.2 .AND. I2.LE.2) THEN
C...         don't change
          ELSE
C...         u and d have opposite definitions
            IF(I1.LE.2) I1=ITABI(I1,2)
            IF(I2.LE.2) I2=ITABI(I2,2)
          ENDIF
          ITRANS=I1*100 + I2*10 + (J1-1)/2
          ITRANS=ISIGN(ITRANS,ID)
C...         charmed and top mesons have wrong sign
          IF(I2.EQ.4 .AND. I1.NE.4) ITRANS=-ITRANS
          IF(I2.EQ.6 .AND. I1.NE.6 .AND. I1.NE.4) ITRANS=-ITRANS
C...check for illegal antiparticles
          IF(I2.EQ.I1 .AND. ID.LT.0) ITRANS=0
C...diquarks
      ELSEIF(I1.EQ.0) THEN
C...         u and d have opposite definitions
          IF(I3.LE.2) I3=ITABI(I3,2)
          IF(I2.LE.2) I2=ITABI(I2,2)
          IF(I3.LT.I2) THEN
            ITRANS=I3*1000 + I2*100 + (J1-1)/2
          ELSE
            ITRANS=I2*1000 + I3*100 + (J1-1)/2
          ENDIF
          ITRANS=ISIGN(ITRANS,ID)
C...         charmed and top mesons have wrong sign
          IF(I2.EQ.4 .AND. I3.NE.4) ITRANS=-ITRANS
          IF(I2.EQ.6 .AND. I3.NE.6 .AND. I3.NE.4) ITRANS=-ITRANS
C...baryons
      ELSE
C...         u and d have opposite definitions
          IF(I3.LE.2) I3=ITABI(I3,2)
          IF(I2.LE.2) I2=ITABI(I2,2)
          IF(I1.LE.2) I1=ITABI(I1,2)
          IF(I3.LE.2) THEN
            ITRANS=I3*1000 + I2*100 + I1*10 + (J1-2)/2
          ELSEIF(I1.LE.2 .AND. I2.LE.2) THEN
            ITRANS=I2*1000 + I1*100 + I3*10 + (J1-2)/2
          ELSE
            ITRANS=I1*1000 + I2*100 + I3*10 + (J1-2)/2
          ENDIF
          ITRANS=ISIGN(ITRANS,ID)
      ENDIF

 300  RETURN
      END
