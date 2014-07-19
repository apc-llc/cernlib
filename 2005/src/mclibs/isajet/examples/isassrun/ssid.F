C     are used in /SSTYPE/.
C
C-----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/sslun.inc"
      CHARACTER*5 LABEL(-120:120)
      SAVE LABEL
      INTEGER ID,J
C
      DATA LABEL(0)/'     '/
C
      DATA (LABEL(J),J=1,10)
     $/'UP   ','DN   ','ST   ','CH   ','BT   ','TP   '
     $,'ERROR','ERROR','GL   ','GM   '/
      DATA (LABEL(J),J=-1,-10,-1)
     $/'UB   ','DB   ','SB   ','CB   ','BB   ','TB   '
     $,'ERROR','ERROR','ERROR','ERROR'/
C
      DATA (LABEL(J),J=11,20)
     $/'NUE  ','E-   ','NUM  ','MU-  ','NUT  ','TAU- '
     $,'ERROR','ERROR','ERROR','ERROR'/
      DATA (LABEL(J),J=-11,-20,-1)
     $/'ANUE ','E+   ','ANUM ','MU+  ','ANUT ','TAU+ '
     $,'ERROR','ERROR','ERROR','ERROR'/
C
      DATA (LABEL(J),J=21,30)
     $/'UPL  ','DNL  ','STL  ','CHL  ','BT1  ','TP1  '
     $,'ERROR','ERROR','GLSS ','Z1SS '/
      DATA (LABEL(J),J=-21,-30,-1)
     $/'UBL  ','DBL  ','SBL  ','CBL  ','BB1  ','TB1  '
     $,'ERROR','ERROR','ERROR','ERROR'/
C
      DATA (LABEL(J),J=31,40)
     $/'NUEL ','EL-  ','NUML ','MUL- ','NUTL ','TAU1-'
     $,'ERROR','ERROR','W1SS+','Z2SS '/
      DATA (LABEL(J),J=-31,-40,-1)
     $/'ANUEL','EL+  ','ANUML','MUL+ ','ANUTL','TAU1+'
     $,'ERROR','ERROR','W1SS-','ERROR'/
C
      DATA (LABEL(J),J=41,50)
     $/'UPR  ','DNR  ','STR  ','CHR  ','BT2  ','TP2  '
     $,'ERROR','ERROR','W2SS+','Z3SS '/
      DATA (LABEL(J),J=-41,-50,-1)
     $/'UBR  ','DBR  ','SBR  ','CBR  ','BB2  ','TB2  '
     $,'ERROR','ERROR','W2SS-','ERROR'/
C
      DATA (LABEL(J),J=51,60)
     $/'NUER ','ER-  ','NUMR ','MUR- ','NUTR ','TAU2-'
     $,'ERROR','ERROR','ERROR','Z4SS '/
      DATA (LABEL(J),J=-51,-60,-1)
     $/'ANUEL','ER+  ','ANUMR','MUR+ ','ANUTR','TAU2+'
     $,'ERROR','ERROR','ERROR','ERROR'/
C
      DATA (LABEL(J),J=82,86)
     $/'HL0  ','HH0  ','HA0  ','ERROR','H+   '/
      DATA LABEL(-86)/'H-   '/
C
      DATA LABEL(80)/'W+   '/,LABEL(-80)/'W-   '/,LABEL(90)/'Z0   '/
      DATA LABEL(91)/'GVSS '/
      DATA LABEL(120)/'PI+  '/,LABEL(-120)/'PI-  '/
C
      IF(IABS(ID).GT.120) THEN
        WRITE(LOUT,*) 'SSID: ID = ',ID
        STOP99
      ENDIF
      SSID=LABEL(ID)
      RETURN
      END
