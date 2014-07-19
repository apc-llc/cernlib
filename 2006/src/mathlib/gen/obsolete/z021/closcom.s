*
* $Id: closcom.s,v 1.1.1.1 1996/04/01 15:03:22 mclareni Exp $
*
* $Log: closcom.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:22  mclareni
* Mathlib gen
*
*
          IDENT CLOSCOM
          ENTRY CLOSUN,CLOSNR
#70C#     EQU  0
          SPACE  2
*  FORTRAN CALLABLE ROUTINE TO DO CLOSE,NR OR CLOSE,UNLOAD ON FILE
*  PARAMETER IS LOGICAL FILE NAME OR FORTRAN NUMBER
*  MAY CLOSE SEQUENTIAL OR FORTRAN RANDOM FILES
CLOSUN.   VFD  42/7LCLOSCOM,18/CLOSUN
SAVE      BSS    1
SAVFIT    BSS    1
MWB       BSS    1
DUMFET    BSS    1                 DUMMY FET FOR CLOSE CPC
          VFD    60/CLOSUN         DUMMY FIRST
          VFD    60/CLOSUN         DUMMY IN
          VFD    60/CLOSUN         DUMMY OUT
          VFD    60/CLOSUN+65      DUMMY LIMIT
          SPACE  2
EXIT      BSS    0
          SA1    SAVE
          SA0    X1
CLOSUN    EQ  $+400000B
          RJ  GETLFN         SAVE A0 AND GET FIT ADRESS,OC FLAG AND WA
          B1=1
          NZ     X2,CLOSCPC     IF CLOSED OR NEVER OPENED  UNLOAD CPC
          NZ  X3,CLOS4       JUMP IF NOT WA
          RJ  CLOSN           CLOSE RANDOM FILE TO WRITE INDEXS
          SA1    SAVFIT
*  ON MET BT=C POUR EVITER RM354 SUR OPENM SUIVANT
          STORE  X1,BT=C
          OPENM  X1,I-O,N
CLOS4     SA1  SAVFIT
          FETCH X1,BAL,X6
          AX6  59
          SA6  MWB            MASK A 7777.. SI BAL=1
          SA2  SAVE
          AX2  30             SEULEMENT ADR 2IEME ARG.
          SX7  #U#
          ZR  X2,NORET
          SX7  #RET#
NORET     STORE X1,CF=X7
          CLOSEM X1
*  CLEANUP FIT AND FET TO SIMULE NEVER OPENED
          SA1  SAVFIT        RESTORE FIT ADRESS
          FETCH  X1,BFS,X3    SAVE BUFFER ADRESS
          FETCH  X1,FWB,X0
          SX7  B0
          SB2  X1+1           FI
          SB3  X1+35
LOPUN     SA7  B2
          SB2  B2+B1
          LT  B2,B3,LOPUN
          SA2  MWB            X2=MASK POUR BUFFER
          BX0  -X2*X0         SI BAL=1 PAS DE BUFFER
          BX3  -X2*X3
          STORE  X1,FWB=X0
          STORE  X1,BFS=X3     STORE BUFFER LENGTH
          STORE  X1,LT=ANY
          STORE  X1,OF=N
          STORE  X1,EO=AD
          STORE X1,CF=N
          SX2  3
          STORE X1,DFC=X2
          MX2  0
          STORE X1,EFC=X2
          EQ     EXIT
          SPACE  2
*
*         FORTRAN CALLABLE ROUTINE TO DO CLOSE NR
*
CLOSNR    EQ  *+400000B
          SA2  CLOSNR
          BX6  X2
          SA6  CLOSUN         SAVE RETURN ADRESS
          RJ  GETLFN         SAVE A0,GET FIT ADRESS,OC FLAG,WA FLAG
          NZ,X2  EXIT              IF FILE NOT OPENED RETURN
          RJ  CLOSN           CLOSE,NR
          SA1  SAVFIT
*  ON MET OC=0 POUR FORCER TRAITEMENT CARTE FILE SUR OPEN SUIVANT
          SA5  X1+17          OC= BIT 57-58 MOT 17
          MX7  2
          LX7  59
          BX7  -X7*X5
          SA7  A5
          SA2  X1+18          ON MET RC=0
          MX0  30
          BX6  X0*X2          POUR EVITER DES BACKSPACES
          SA6  A2             LORS DE L OPEN FORTRAN SUIVANT
          EQ     EXIT              RETURN
          SPACE  2
CLOSCPC   BSS    0
*         ENTRY WITH X1 = ADD OF FIT
          MX0    42
          SA1    X1                GET FIRST WORD OF FIT FOR LFN
          SX2    1
          BX3    X0*X1            ISOLATE LFN
          BX6    X3+X2             SET COMPLETE BIT
          SA6    DUMFET           STORE LFN AND COMPLETE BIT
          CLOSE  DUMFET,UNLOAD,RECALL
          EQ     EXIT
          SPACE  2
*   ON ENTRY X1  ADRESSWORD WITH LFN OR FORTRAN NUMBER
*  EXIT  FIT ADRESS IN X1 AND SAVFIT,A0 IN SAVE
*    OC FLAGS IN X2  AND X3=0 IF WA
GETLFN    BSSZ  1
          SX6  A0
          SB1   1
          B1=1
          SA2  A1+B1          X2= ADR 2IEME ARG.
          SX2  X2             SEULEMENT 18 BITS
          LX2  30
          BX6  X6+X2          ADR + A0
          SA6  SAVE            SAVE  A0
#if !defined(CERNLIB_F4)
          MX7    1
          BX1    X7+X1
#endif
+RJ=XGETFIT
-         VFD  30/CLOSUN.
          BX6  X1            NEVER OPENED   SAVE FIT
          SA6  SAVFIT
          FETCH  X1,OC,X2
          SX2    X2-1              X2=0 IF OPENED
          MX4  0
          NZ,X2  GETLFN            IF FILE NOT OPENED RETURN
          STORE X1,EX=ERRCL
          STORE  X1,DX=X4
          FETCH  X1,FO,X3
          SX3  X3-#WA#         SET X3 TO 0 IF WA
          EQ  GETLFN
          SPACE  2
*  CLOSE,NR FILE  IF WA FETCH IF RJ STORED IN CLSLNK.  IF SO GO
*  EXECUTE ROUTINE
CLOSN     BSSZ  1
          NZ  X2,CLOSN        FILE NOT OPENED  RETURN
          ZR  X3,CLOSN2      JUMP IF RANDOM
CLOSN1    SA2  SAVE
          AX2  30             SEULEMENT ADR 2IEME ARG.
          SX7  #N#
          ZR   X2,NODET
          SX7  #DET#
NODET     STORE X1,CF=X7
          CLOSEM X1
          EQ  CLOSN
CLOSN2    SA2  CLOSN3
          LX2  12
          NG   X2,CLOSN1
CLOSN3    RJ   =YCLOS.RI
-         VFD  30/CLOSUN.
          EQ  CLOSN
          SPACE  2
ERRCL     BSSZ  1
          RJ  =XIOERR.
-         VFD  30/CLOSUN.
          END
