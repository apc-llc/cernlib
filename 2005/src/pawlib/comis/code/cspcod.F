*
* $Id: cspcod.F,v 1.1.1.1 1996/02/26 17:16:28 mclareni Exp $
*
* $Log: cspcod.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_CSDEBUG)
*CMZ :  1.17/07 20/12/93  17.39.36  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSPCOD(LUNP,SNAME)
***--------------------------------------------
*       prints codes of routine sname
***--------------------------------------------
      CHARACTER *(*) SNAME
        PARAMETER (LHP=50006, NBYTPW=4, KDLEN=2, KLCMLX=2)
      COMMON/MDPOOL/IQ(LHP)
      INTEGER KD(LHP),KD1(99),KD2(99),KD3(99)
      EQUIVALENCE (IQ,KD)
      EQUIVALENCE (KD1(1),KD(2)),(KD2(1),KD(3)),(KD3(1),KD(4))
      INTEGER CSADDR
      DOUBLE PRECISION DP
      COMPLEX CX
      CHARACTER*8 NAMES(208), TXT*120, CHT*1
      DATA NAMES/'I.OR.I','R.OR.R','I.AND.I','R.AND.R','.NOT.I',
*                   1         2        3         4        5
     +'.NOT.R','I.EQ.I','R.EQ.R','CH.EQ.CH','I.NE.I','R.NE.R',
*         6       7        8         9         10       11
     +'CH.NE.CH','I.LT.I','R.LT.R','CH.LT.CH','I.LE.I','R.LE.R',
*         12        13       14        15        16       17
     +'CH.LE.CH','I.GE.I','R.GE.R','CH.GE.CH','I.GT.I','R.GT.R',
*         18        19       20        21        22       23
     +'CH.GT.CH','-I','-R','REAL(I)','INT(R)','REAL2(I)','INT2(R)',
*         24      25   26    27       28       29         30
     +'I+I','R+R','I-I','R-R','I*I','R*R','I/I','R/R','I**I','R**I',
*      31    32    33    34    35    36    37    38    39     40
     +'R**R','A=A','CH=CH','CH//CH','LK','LAK','LCK','LACK','DCV',
*      41     42    43      44       45   46    47    48     49
     +'LVL','LAL','LVG','LAG','LVP','LAP','LVKA','LAKA','LVCL',
*      50    51    52    53    54    55    56     57     58
     +'LACL','LVCG','LACG','LVCP','LACP','I**R','I**D','R**D',
*      59     60     61     62     63     64     65     66
     +'IFUN1','IFUN2','IFUNN','DFA','DFAS','DFCA','DFCAS','LEA','LAA',
*      67      68      69      70    71     72     73      74    75
     +'LHK','LAHK','BBLK','EBLK','LECA','LACA','OPEN','STO',
*      76    77     78     79     80     81      82     83
     +'STOC','SETPC','AA','CALLS','IOEND','ACA','ASSGO','GOI','CALL',
*      84     85      86   87      88      89    90      91    92
     +'RET','GO','GOC','BRZI','BRZR','ENTRY','DOI','DOR','ODI','ODR',
*      93    94   95    96     97     98      99    100   101   102
     +'PAUSE','FARGL','FCALL','NUM','INP','OUT','CONT','JMPT','SVL',
*      103     104     105     106   107   108   109    110    111
     +'RETM','EXIT','PUSH','LPB','D==D','D<>D','D<D','D<=D','D>=D',
*      112    113    114    115   116    117    118   119    120
     +'D>D','-D','D(I)','D(R)','I(DP)','R(DP)','DP2(I)','DP2(R)','D+D',
*      121   122  123    124    125     126     127      128      129
     +'D-D','D*D','D/D','D**D','D**I','D**R','D=D','LDK','LADK','LVDL',
*      130   131   132   133    134    135    136   137   138    139
     +'LADL','LVDG','LADG','LVDP','LADP','LBAK','LBA2K','LEDAL','LADAL',
*      140    141    142    143    144    145    146    147     148
     +'WRITE','READ','IOV','IOA','SVDL','IFAI','IFAR','IFAD','CLOSE',
*      149     150    151   152   153    154    155    156    157
     +'REWIND','BACKSPAC','ENDFILE','INQUIRE','ill','CX==CX','CX<>CX',
*      158      159        160       161       162   163      164
     +'-CX','CX(I)','CX(R)','CX(D)','I(CX)','R(CX)','D(CX)','CX2(I)',
*      165   166     167     168     169     170     171     172
     +'CX2(R)','CX2(D)','CX+CX','CX-CX','CX*CX','CX/CX','CX**CX',
*      173      174      175     176     177     178     179
     +'CX**I','CX**R','CX**D','CX=CX','LCXK','LACXK','LVCXL','LACXL',
*      180     181     182     183     184    185     186     187
     +'LVCXG','LACXG','LVCXP','LACXP','LBAL','LBAG','LECXAL',
*      188     189     190     191     192     193     194
     +'LACXAL','SVCXL','I**CX','R**CX','D**CX','LEKV','LAKV',
*      195      196     197     198     199      200    201
     +'LBAKA','LBAA','LBACK','LBACL','LBACG','BACA','LBAHK'   /
*      202     203    204     205     206     207    208
*=============================================================
      IPC=CSADDR(SNAME)
      IF(IPC.LE.0)THEN
         PRINT *,'   missing routine ',SNAME
         RETURN
      ENDIF
      CALL CSTYPE(IPC,CHT)
      IPC=IQ(IPC+2)
      IPCB=IPC
      LASTC=IPC+KD1(IPC)-1
      JS=MJSCHA(TXT)
      N=3
      IFP=1
      WRITE(LUNP,*)'   codes of routine ',SNAME,'.',CHT,'  at pc=',IPC
 998  I=KD(IPC)
      IF(IFP.NE.0)THEN
        IF(N.EQ.1)THEN
          WRITE(LUNP,1234)IPC-IPCB,NAMES(I)
        ELSE
          WRITE(LUNP,1235)IPC-IPCB,NAMES(I),(KD(K),K=IPC+1,IPC+N-1)
        ENDIF
      ENDIF
 1234 FORMAT(2X,I6,2X,A)
 1235 FORMAT(2X,I6,2X,A,4(6I10))
      IPC=IPC+N
      IF(IPC.GT.LASTC) GO TO 999
      IFP=1
*
       GO TO(1,1,1,1,1,1,1,1,1, 1, 1, 1, 1, 1, 1,
*            1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
     1  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
*      16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
     2  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,45,
*      33,34,35,36,37,38,39,40,41,42,43,44,45,
     3 45,77,77, 2, 2, 2,52,53, 2, 2, 3, 3, 3, 3,60,
*      46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
     4 61, 3, 3, 1, 1, 1, 3, 3, 4, 3, 3, 4, 4, 3, 3,76,76,
*      61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,
     5  4, 1, 3, 3, 8, 2, 3, 2, 2,87, 1, 2, 3, 2,92, 1,
*      78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,
     6  2,95, 2, 2, 3, 6,  6,  3,  3,103,  2,  4,  2,107,
*      94,95,96,97,98,99,100,101,102,103,104,105,106,107,
     7  108,  1, 76,  2,  1,  1,  2,  2
*       108,109,110,111,112,113,114,115
     A ,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
*      ,116,117,118,119,120,121,122,123,124,125,126,127,
     B    1,  1,  1,  1,  1,  1,  1,  1,  1,137,137,  2,
*       128,129,130,131,132,133,134,135,136,137,138,139,
     C    2,141,142,  2,  2,  2,  3,  3,  3,  5,  5,  2,
*       140,141,142,143,144,145,146,147,148,149,150,151,
     D    3,  2,  4,  4,  4,  1,  1,  1,  1,  3,700
*       152,153,154,155,156,157,158,159,160,161,162
     1 ,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
*      ,163,164,165,166,167,168,169,170,171,172,173,
     2    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,184,184,  2,  2,
*       174,175,176,177,178,179,180,181,182,183,184,185,186,187,
     3  188,189,  2,  2,  2,  3,  3,  3,  2,  1,  1,  1, 4,  4,
*       188,189,190,191,192,193,194,195,196,197,198,199,200,201
     4    3,  2, 77,  3,  4,  2, 76
*       202,203,204,205,206,207,208
     8                    ),KD(IPC)
      GO TO 700
*     ---------
 1    N=1
      GO TO 998
 2    N=2
      GO TO 998
 3    N=3
      GO TO 998
 4    N=4
      GO TO 998
 5    N=5
      GO TO 998
 6    N=6
      GO TO 998
 8    N=8
      GO TO 998
*LHK NW,TEXT
 76   N=KD1(IPC)+2
      IFP=0
      I=KD(IPC)
      NB=KD1(IPC)*NBYTPW
      nb=min(nb,119)
      CALL CCOPYS(MJCHAR(KD2(IPC)),JS,NB)
      WRITE(LUNP,1236)IPC-IPCB,NAMES(I),TXT(1:NB)
 1236 FORMAT(2X,I6,2X,A,2X,A)
      GO TO 998
*LC   NB,'TEXT'
  77  N=(KD1(IPC)-1)/NBYTPW+3
      IFP=0
      I=KD(IPC)
      NB=KD1(IPC)
      nb=min(nb,119)
      CALL CCOPYS(MJCHAR(KD2(IPC)),JS,NB)
      WRITE(LUNP,1236)IPC-IPCB,NAMES(I),TXT(1:NB)
      GO TO 998
*LK (I OR R) CONST
 45   N=2
      IFP=0
      I=KD(IPC)
      CALL CCOPYA(KD1(IPC),RV,1)
      WRITE(LUNP,1246)IPC-IPCB,NAMES(I),KD1(IPC),RV
 1246 FORMAT(2X,I6,2X,A,2X,I12,2X,E14.7)
      GO TO 998
*LVG N,I
  52  N=3
      GO TO 998
*LAG N,I
  53  GO TO 52
*LVCG N,NB,I
  60  N=4
      GO TO 998
*LACG N,NB,I
  61  GO TO 60
*CALLS IP,NPAR,ITB,...
  87  KS=1
      GO TO 9201
*CALL  IP,NPAR[,IHK,IKK,ITB]*NPAR
  92  KS=3
 9201 N=KD2(IPC)*KS+3
      GO TO 998
*GOC N,L1,...,LN
  95  N=KD1(IPC)+2
      GO TO 998
*PAUSE NCH,TEXT
  103 NCH=KD(IPC+1)
      N=2
      IF(NCH.GT.0)N=(NCH-1)/NBYTPW+3
      GO TO 998
*INP 0,NPAR[,IE,IT]*NPAR
 107  N=KD2(IPC)*2+3
      GO TO 998
*TYP 0,NPAR[,IE,IT]*NPAR
 108  GO TO 107
*LDK DK
  137 N=KDLEN+1
      IFP=0
      I=KD(IPC)
      CALL CCOPYA(KD1(IPC),DP,KDLEN)
      WRITE(LUNP,1237) IPC-IPCB,NAMES(I),DP
 1237 FORMAT(2X,I6,2X,A,2X,D14.7)
      GO TO 998
*LVDG N,I
  141 GO TO 52
*LADG N,I
  142 GO TO 52
*LCXK CXK
 184  N=1+KLCMLX
      IFP=0
      I=KD(IPC)
      CALL CCOPYA(KD1(IPC),CX,KLCMLX)
      WRITE(LUNP,1238) IPC-IPCB,NAMES(I),CX
 1238 FORMAT(2X,I6,2X,A,2X,'(',E14.7,',',E14.7,')')
      GO TO 998
*LVCXG N,I
  188 GO TO 52
*LACXG N,I
  189 GO TO 52
 700  WRITE(LUNP,1239) IPC-IPCB,KD(IPC)
 1239 FORMAT(2X,' unknown code: ' I6,2X,I12)
 999  CONTINUE
      END
#endif
