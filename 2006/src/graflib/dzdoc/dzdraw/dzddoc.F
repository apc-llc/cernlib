*
* $Id: dzddoc.F,v 1.2 1997/03/14 14:20:32 mclareni Exp $
*
* $Log: dzddoc.F,v $
* Revision 1.2  1997/03/14 14:20:32  mclareni
* WNT mods
*
* Revision 1.1.1.1.2.1  1997/01/21 11:24:57  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/03/04 16:13:17  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDDOC(IXSTOR,L,CHBANK,CHOPT,IF1,IL1,LUN,CHPF,NKEEPS)
*.
*. INPUT     :  IXSTOR  store index
*.              L       link to bank to be dumped
*.              CHOPT   character option
*.                      'U' suppress list of UNDEFIND
*.                      'K' Keep sequences
*.                      'D' INTEGER statements for data offsets
*.                      'O' Parameter statements for data offsets
*.                      'Z' force hexadecimal (DZSHOW)
*.                      'T' list also pure Character banks 1 word/line
*.                      'C'  list value only (useful for output read by
*.                           another program
*.                      '='  list as IQ(LCHBANK(1)+IOFF)=value
*.                      'R'  try to recover from incomplete doc
*.                      'V'  return data word values into VVAR
*.                      'P'  return path to data word into VVAR
*.                      'I'  return IO char into CHPF
*.                      'E'  examine, check range
*.              IFIRST   first word to dump
*.              ILAST    last word
*.              LUN      output unit
*.              CHPF     prefix when making FORTRAN code (data words)
*.              NKEEPS   # of data words for which offsets
*.                       have been gen
*. OUTPUT    :
*.
*. CALLS     : DZSHOW UOPTC ,MZDROP,RZIN,UCTOH, DZSHOW
*. CALLED    : DZDISP, USER
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 7-Aug-88
*. LAST MOD  : 18-OCT-92
*.**********************************************************************
      CHARACTER*(*) CHOPT, CHPF
      CHARACTER*40 CHPFI
      CHARACTER*8  CHBANK
      INTEGER KEYVEC(2), IWHOLL, NLSKIP,IDELAY
      CHARACTER*11 CVAL, CVALSA
      CHARACTER*21 CLOOP
      CHARACTER*1 COPTDZ
      CHARACTER*80 CLINE, CLINSA, CLINDE
      CHARACTER*8 CVAR, CVAR1
      CHARACTER*12 CTEMP
      SAVE LUNINT, IOCH
      PARAMETER (LUNINV=41)
      LOGICAL NEWVAL,KEEPSQ,INHOLL,REPEND, NEWPNT, LRECOV, ENDED
*     pointer stuff
      INTEGER MAXP, IPOINT
      PARAMETER (MAXP=200)
      CHARACTER*8 CVARP, CHREPC
      CHARACTER*8 CPOINT(MAXP)
      INTEGER POINT(MAXP),NPOINT(MAXP), IWDOC,IWDOCO,IWORD, IF11,IL11,
     +        NSTYP, IPCHPF
      CHARACTER*1 CHARIO,CHARIS
      CHARACTER*7 CHARIA
      CHARACTER*32 CVARNM
 
*     for returning values
      INTEGER VVAR(100), NVAL,ITY, MAXVAL
      INTEGER VALINT
      REAL    VALREA
      EQUIVALENCE (VALINT,VALREA)
      CHARACTER*8 CVREQ
      LOGICAL REPATH, NOTFIL
      CHARACTER*13 SPACES
      EXTERNAL SPACES
#if defined(CERNLIB_NONEWL)
#include "dzdoc/nonewl2.inc"
#endif
#if !defined(CERNLIB_NONEWL)
#include "dzdoc/nonewl1.inc"
#endif
*
#include "dzdoc/bkwrp.inc"
#include "dzdoc/tapes.inc"
*         From DZEBRA
#include "zebra/zbcdk.inc"
#include "zebra/zunit.inc"
#include "dzdprm.inc"
#include "dzdoc/linout.inc"
#include "dzdoc/docparq.inc"
      CHARACTER CQSTAK*13,CQINFO*40
      PARAMETER (NLICHQ=130,NSTCHQ=8,NDVCHQ=8,NBKCHQ=4 )
      CHARACTER CQLINE*(NLICHQ),CQMAP(10)*(NLICHQ)
      CHARACTER CQSTOR*(NSTCHQ),CQDIV*(NDVCHQ),CQID*(NBKCHQ)
      COMMON /DZC1CH/ CQSTOR,CQDIV,CQID,CQMAP,CQSTAK,CQINFO
      EQUIVALENCE (CQLINE,CQMAP)
#include "dzdoc/bknuparq.inc"
#include "zebra/bkfoparq.inc"
#include "dzdoc/bkstparq.inc"
#include "dzdoc/bktgparq.inc"
#include "dzdoc/bktgdatq.inc"
*
      DATA CHARIA/'BIFDHUR'/
      DATA LUNINT/0/
*------
      IENTRY=1
      ITY=0
      IF11=IF1
      IL11=IL1
      IF(INDEX(CHOPT,'K').NE.0 .OR. INDEX(CHOPT,'I').NE.0)THEN
         KEEPSQ=.TRUE.
      ELSE
#include "zebra/qstore.inc"
         IF(IL11.GT.IQ(KQS+L-1))IL11=IQ(KQS+L-1)
         KEEPSQ=.FALSE.
      ENDIF
      NSTYP=0
      CHARIS=' '
      CHARIO=' '
      IF(INDEX(CHOPT,'D').NE.0)THEN
         IF(INDEX(CHOPT,'R').NE.0)THEN
            CHARIO='R'
         ELSE
            CHARIO='I'
         ENDIF
      ENDIF
      IF(INDEX(CHOPT,'I').NE.0)THEN
         IPCHPF=1
         CHPF=' '
      ELSE
         CALL UCTOH(CHBANK,KEYVEC,4,8)
      ENDIF
      LUC = LUN
      REPATH=.FALSE.
      MAXVAL=0
      GOTO 2
 
      ENTRY DZDGVA(IXSTOR, L, CHOPT,CVREQ,NVAL,VVAR,ITYYY)
      IF(L.EQ.0)GOTO 990
#include "zebra/qstore.inc"
      LUC = 6
      IF11=1
      IL11=IQ(KQS+L-1)
      MAXVAL=NVAL
      NVAL=0
      IENTRY=2
      ITY=-1
      IPREP0=0
      IPREP1=0
      IPDRE0=0
      IPDRE1=0
      NOTFIL=.TRUE.
      IF(INDEX(CHOPT,'P').NE.0)THEN
         REPATH=.TRUE.
      ELSE
         REPATH=.FALSE.
      ENDIF
      KEYVEC(1)=IQ(KQS+L-4)
      LUP=LQ(KQS+L+1)
      IF(LUP.NE.0)THEN
         KEYVEC(2)=IQ(KQS+LUP-4)
      ELSE
         CALL UCTOH('NONE',KEYVEC(2),4,4)
      ENDIF
 
2     LUNINT = LUNINV
      LUNSAV = IQPRNT
      IDENTF=0
      IWDOCO=0
      NEXTRA=0
      IMBED2=0
      IOCH = -1
      IPOINT=0
      DO 5 I=1,MAXP
         POINT(I)  = -1
         CPOINT(I) = ' '
5        NPOINT(I) = -1
 
*      IREPC=-2
      IPRKEE=0
      NLSKIP=0
      IDELAY=0
      CLINSA=' '
      LCLINE  = LEN(CLINE)
      IPLINE = 1
      INHOLL = .FALSE.
      IF(INDEX(CHOPT,'R').NE.0)THEN
         LRECOV=.TRUE.
      ELSE
         LRECOV=.FALSE.
      ENDIF
      ENDED =.FALSE.
      REPEND=.FALSE.
      NEWPNT=.FALSE.
      IF(KEEPSQ)THEN
          NBLENG = IL1
      ELSE
         NBLENG = IQ(KQS+L-1)
         KEEPSQ=.FALSE.
*         CALL CLTOU(CHOPT)
         IF(INDEX(CHOPT,'Z').NE.0)THEN
            COPTDZ='Z'
         ELSE
            COPTDZ=' '
         ENDIF
         CALL UCTOH ('$LF/',IILFLF,4,4)
      ENDIF
      IF(INDEX(CHOPT,'C').NE.0 .AND. .NOT.KEEPSQ)THEN
         IQPRNT=LUC
         IW1=IF11
         GOTO 90
      ENDIF
      IF(INDEX(CHOPT,'I').EQ.0)
     +CALL  DZDGDO(0,LQBKD1,KEYVEC,CLINE,IFC,ILC,ICYCLE)
      IF(LQBKD1.EQ.0)THEN
         IF(KEEPSQ  .OR. INDEX(CHOPT,'V').NE.0)THEN
            WRITE(LUC,'(A)')'*   No documentation for '//CHBANK(1:4)
            GOTO 990
         ENDIF
         NDATA = 0
      ELSE
 
         IF(ICYCLE.LT.0)WRITE(LUC,'(A)')
     &  ' **** Requested version not found ****'
         IPDATA = IQ(KQS+LQBKD1+MBHEAQ) + IQ(KQS+LQBKD1+MBGENQ)
     +           +IQ(KQS+LQBKD1+MBLINQ) + IQ(KQS+LQBKD1+MBRLIQ)
     &           +IQ(KQS+LQBKD1+MBBITQ) + 1
         IP1 = 0
         IP2 = 0
         IC1 = 0
         IC2 = 0
         IWORD = 0
         IWORDR=0
         IREPL=0
         IFREPL = 0
         CHREPC=' '
         ILOWRC=0
         IUPERC=0
         LOOPC1=-1
         LOOPC2=-1
         IPFORM=0
         ICFORM=0
         IMFORM=0
         LALILE=0
         IP = KQS+LQBKD1+IPDATA
         IPSAVE=IP
         NDATA = IQ(KQS+LQBKD1+MBDATQ)
      ENDIF
 
      IF(INDEX(CHOPT,'Q').EQ.0 .AND.
     &   INDEX(CHOPT,'V').EQ.0 .AND.
     &  .NOT.KEEPSQ)THEN
         IF(INDEX(CHOPT,'E').EQ.0)THEN
            WRITE(LUC,'(10A)')
     &' -------- Data of Bank/UpBank: ',CHBANK(1:4),'/',CHBANK(5:8),
     &     ' Doc Version: ',CLINE(IFC:ILC), ' ----------'
         ELSE
            WRITE(LUC,'(10A)')
     &   '  Checking data of Bank/UpBank: ',CHBANK(1:4),'/',CHBANK(5:8)
         ENDIF
      ENDIF
 
      IF(NDATA.GT.0)THEN
          IC9 = 10
      ELSE
          IQPRNT = LUC
          IW1 = IF11
          WRITE(LUC,'(A)')
     +    '* Data words not documented for '//CHBANK(1:4)
          IF(KEEPSQ .OR. INDEX(CHOPT,'V').NE.0)THEN
             GOTO 990
          ELSE
             GOTO 90
          ENDIF
      ENDIF
      LDATUM = IP + NDATA
      IWDOC = 0
 
*      IF(.NOT.KEEPSQ) WRITE(LUC,*)' '
*
*     IF11,IL11 first, last word to be doc'td
*     IWORD   current word in data
*     IWORDR   current word in rep section
*     IWDOC   word announced in documentation
*     IWDOCO  remember announced word (for cont lines)
*     IP      pointer in documentation
*     IP1     remember IP for start of rep lev 1
*     IP2     remember IP for start of rep lev 2
*     IC1,IC2 repetition counter 1, 2
*     IT      type of info in doc
*             2 integer
*             5 holl
*     NW      # of words in doc
*     IDENT   ITGREQ describe rep
*             ITGENQ describe seq # of data
*     NEWVAL  if false: cont card
*     INHOLL  holl text in data words started
*     IPFORM  pointer to a key descriptor format (see RZ)
*     LOOPC1  or LOOPC2)loop counters in rep section
*
 
 
   10 CONTINUE
      IF(IP .GE. LDATUM)THEN
          IF(.NOT.KEEPSQ)THEN
              IQPRNT = LUC
               ENDED=.TRUE.
               NEWVAL=.TRUE.
               IP=IPSAVE
          ENDIF
          IF(KEEPSQ)GOTO 990
      ENDIF
      IT = MOD(IQ(IP),16)
      NW = IQ(IP)/16
*      WRITE(*,*)' IT,NW,IP ',IT,NW,IP
      IF(NW .LE. 0)THEN
         WRITE(LUC,'(A,2I9)')' Illegal NW at IPDATA ',NW,IPDATA
         GOTO 990
      ENDIF
 
*     start hollerith sector
 
      IF(IT .EQ. 5)THEN
*         WRITE(*,*)' IDENTF= ',IDENTF
*        try to get rep count name and range
         IF(KEEPSQ .AND. ABS(IDENTF).EQ. ITGREQ)THEN
            NCH=MIN(NW*4,LEN(CLINE))
            CLINE=' '
            CALL UHTOC(IQ(IP+1),4,CLINE,NCH)
            NCH=LNBLNK(CLINE)
            ICOLON=INDEX(CLINE(1:8),':')+1
            CHREPC=CLINE(ICOLON:8)
            IFC=INDEX(CLINE,'[')
            ILC=INDEX(CLINE,']')
            IFC=IFC+1
            ILC=ILC-1
            NC=INDEX(CLINE(IFC:ILC),',')
            IF(NC.EQ.0)NC=INDEX(CLINE(IFC:ILC),':')
            IF(IFC.GT.1 .AND. ILC.GT.IFC+2 .AND. NC.NE.0)THEN
               ILC1=IFC+NC-2
               IFC2=ILC1+2
               CALL DZDCTI(CLINE(IFC:ILC1),ILOWRC)
               CALL DZDCTI(CLINE(IFC2:ILC),IUPERC)
            ENDIF
         ENDIF
         IF(IDENTF .NE. ITGENQ) GOTO 80
 
*        rep count = 0 calc from data word
*         IF(IREPC .EQ. 0)GOTO 80
 
         IF(      IWORD+1 .NE. IWDOC
     +      .AND. IP1+IP2 .EQ. 0
     +      .AND. IWDOC   .NE. IWDOCO )THEN
              IWDOCO = -1
         ENDIF
 
*        rep section ended already?
 
         IF(REPEND)GOTO 80
 
*        look if more words documented then announced
 
         IF(NEXTRA.LT.0 .AND. LRECOV)THEN
            IF(IWDOC.GT.ABS(NEXTRA))THEN
               GOTO 80
            ENDIF
         ENDIF
 
*        is it not continuation card for same item
*        or not a start of rep section
 
         NEWVAL = .FALSE.
*         WRITE(*,*)'IWDOCO ,IWDOC, IWORD', IWDOCO ,IWDOC, IWORD
         IF(IWDOCO .NE. IWDOC .OR. IWDOCO .LE. 0 .OR. NEWPNT)THEN
            IWORD = IWORD+1
            IWORDR = IWORDR+1
            NEWVAL = .TRUE.
         ENDIF
*        remember possible repetition count
         IVALLW = IQ(KQS+L+IWORD)
 
         IF(IWORD .GT. IL11)GOTO 990
         IF(IWORD .LT. IF11)GOTO 80
 
*         look if all words are hollerith
 
         IF(IOCH .LT. 0)THEN
            NIO = JBYT(IQ(KQS+L),19,4)
            NL =  IQ(KQS+L-3)
            IOFFBS = - (NIO + NL + 8 + 1)
            IOCH = JBYT(IQ(KQS+L+IOFFBS),17,16)
            IF(IOCH .EQ. 5 .AND. INDEX(CHOPT,'Z').EQ. 0
     +      .AND. INDEX(CHOPT,'T').EQ. 0)THEN
               IF(.NOT.KEEPSQ)THEN
                   WRITE(LUC,'(1X,2A4,1X,100(15A4/7X))')
     +             (IQ(IP+K),K=1,NW)
                   WRITE(LUC,'(A)')'---- '
               ENDIF
               NEWVAL = .TRUE.
            ENDIF
         ENDIF
 
*        loop here if all words are hollerith
 
   20    CONTINUE
*        empty text buffer at end
          IF(IWORD .GT. IL11)THEN
            IF(IPLINE .GT. 1)THEN
               IF(.NOT.KEEPSQ)
     +          WRITE(LUC,'(14X,A)')CLINE(1:IPLINE-1)
                IPLINE = 1
            ENDIF
            GOTO 990
         ENDIF
 
*        get the printed value from CQLINE of DZSHOW  (Quiet option)
         IF(NEWVAL     .AND.
     &     .NOT.KEEPSQ .OR.
     &     (NEWVAL .AND. INDEX(CHOPT,'V').NE.0 .AND. ITY.EQ.-1)
     &     )THEN
             IF(IC9 .GE. 9 .OR. NEWPNT)THEN
                CALL DZSHOW(' ',IXSTOR, L,COPTDZ//'Q',1,0,IWORD,IWORD+8)
                IC9 = 1
             ELSE
                IC9 = IC9+1
             ENDIF
             IBV1 = IC9*12
             IBV2 = IBV1+10
             CVAL = CQLINE(IBV1:IBV2)
             CVALSA=CVAL
         ELSE
             CVAL = '          '
         ENDIF
*        collect text on CLINE
         IF(IOCH .EQ. 5            .AND.
     +      INDEX(CHOPT,'Z').EQ.0  .AND.
     +      INDEX(CHOPT,'T').EQ. 0 )THEN
            IF(IPLINE .GE. LCLINE)THEN
               IF(.NOT.KEEPSQ)
     +         WRITE(LUC,'(14X,A)')CLINE(1:IPLINE-1)
               IPLINE = 1
            ENDIF
            CLINE(IPLINE:IPLINE+3)=CVAL(8:11)
            IPLINE = IPLINE+4
            IF(INHOLL)THEN
               IWORD = IWORD+1
               GOTO 20
            ELSE
               INHOLL = .TRUE.
               CVAL = ' '
            ENDIF
         ELSE
 
*        output other then pure text, empty text buffer
 
             IF(IPLINE .GT. 1)THEN
                IF(.NOT.KEEPSQ .AND.INDEX(CHOPT,'V').EQ.0)
     &          WRITE(LUC,'(14X,A)')CLINE(1:IPLINE-1)
                IPLINE= 1
             ENDIF
             INHOLL = .FALSE.
         ENDIF
 
         CALL UHTOC(IQ(IP+1),4,CVAR,8)
 
*        return value?
 
         IF(INDEX(CHOPT,'V').NE.0)THEN
            IF(CVAR.EQ.CVREQ)THEN
*              find data type
               IF(ITY.LT.0)THEN
                  IF     (INDEX(CVAL,'"').NE.0)THEN
                     ITY=5
                  ELSE IF(INDEX(CVAL,'.').NE.0)THEN
                     ITY=3
                  ELSE
                     ITY=2
                  ENDIF
               ENDIF
*              return path only?
               IF(REPATH)THEN
                  IF(NOTFIL)THEN
                     NOTFIL=.FALSE.
                     NVAL=NVAL+1
                     IF(IREPL.LE.1)THEN
                        VVAR(NVAL)=IWORD-IPREP0
                     ELSE
                        VVAR(NVAL)=IWORD-IPREP1
                     ENDIF
                  ENDIF
               ELSE
                  IF(ITY.EQ.5)THEN
                     WRITE(*,*)'Holleriths not yet supported'
                     ITY=5
                     GOTO 80
                  ENDIF
                  NVAL=NVAL+1
                  VVAR(NVAL)=IQ(KQS+L+IWORD)
               ENDIF
            ENDIF
            IF(NVAL.EQ.MAXVAL)THEN
               IF(MAXVAL.GT.1)WRITE(*,*)
     &         'Max number of values reached', MAXVAL
                GOTO 990
            ELSE
               GOTO 80
            ENDIF
         ENDIF
 
         CVAR1 = CVAR
         CALL CLTOU(CVAR1)
 
*        conVert it by ZITOH?
 
         IF     (CVAR1(1:2).EQ.'Z:')THEN
            CALL ZITOH(IQ(KQS+L+IWORD),IWHOLL,1)
            CVAL=' '
            CALL UHTOC(IWHOLL,4,CVAL,4)
*        format of a Key descriptor
         ELSE IF(CVAR1(1:2).EQ.'F:')THEN
            IF(IPFORM.EQ.0)IPFORM=KQS+L+IWORD
            DO 15 I=1,10
               IFM=JBYT(IQ(KQS+L+IWORD),(I-1)*3+1,3)
               IF     (IFM.EQ.1)THEN
                  CVAL(I:I)='I'
                  IMFORM=IMFORM+1
               ELSE IF(IFM.EQ.3)THEN
                  CVAL(I:I)='H'
                  IMFORM=IMFORM+1
                ELSE
                  CVAL(I:I)=' '
               ENDIF
               CVAL(11:11)=' '
15          CONTINUE
         ELSE IF(CVAR1(1:2).EQ.'K:')THEN
            ICFORM=ICFORM+1
            IF(ICFORM.GT.IMFORM)ICFORM=1
            IFC2=ICFORM/10
            IFC1=MOD(ICFORM,10)
            IF(IFC1.EQ.0)THEN
               IFC1=10
               IFC2=IFC2-1
            ENDIF
            IFM=JBYT(IQ(IPFORM+IFC2),(IFC1-1)*3+1,3)
            IF(IFM.EQ.3)THEN
               CALL ZITOH(IQ(KQS+L+IWORD),IWHOLL,1)
               CVAL=' '
               CALL UHTOC(IWHOLL,4,CVAL,4)
            ENDIF
         ELSE IF(CVAR1(1:2).EQ.'B:')THEN
             WRITE(CVAL,'(Z9)')IQ(KQS+L+IWORD)
             CVALSA=CVAL
         ELSE IF(CVAR1(1:2).EQ.'D:')THEN
             CALL RZDATE(IQ(KQS+L+IWORD),IDATX,ITIMX,1)
             WRITE(CVAL(1:6),'(I6)')MOD(IDATX,1000000)
             WRITE(CVAL(8:11),'(I4)')MOD(ITIMX,10000)
             CVAL(7:7)='/'
         ENDIF
*     pointer stuff
 
*     look if a label is reached
 
      IF(NEWPNT .OR. .NOT. NEWVAL)THEN
        NEWPNT=.FALSE.
      ELSE
       IF(IPOINT.GT.0)THEN
         DO I=1,IPOINT
            IF(IWORD.EQ.POINT(I) .AND. NPOINT(I).NE.0)THEN
*           skip in doc until label is found
               IF(ENDED)ENDED=.FALSE.
               IPP=IPSAVE
               NWP=-1
21             IPP=IPP+NWP+1
               IF(IPP.GE.LDATUM)THEN
                  WRITE(*,*)'No label found for : ',CPOINT(I)
                  GOTO 26
               ENDIF
               ITP = MOD(IQ(IPP),16)
               NWP = IQ(IPP)/16
               IF(ITP.NE.5)GOTO 21
               CALL UHTOC(IQ(IPP+1),4,CVARP,8)
               CALL CLTOU(CVARP)
               IF(CVARP(1:2).NE.'L:')GOTO 21
 
               IF(CVARP(3:).EQ.CPOINT(I)(3:))THEN
*                 remember for later in case identical lines are skipped
                  WRITE(CLINDE,'(2A4,13X,14A4)')
     +            (IQ(IPP+K),K=1,NWP)
                  IDELAY=1
                  IP=IPP+NWP+1
                  NEWPNT=.TRUE.
*                 end a possible infinite repetition
                  IF(IFREPL.EQ.0)THEN
                     IWORD=IWORD-1
                  ELSE
                     IWORD=IWORD-1
                     IFREPL=0
                  ENDIF
*                  WRITE(*,*)'Label found',IWORD,' ',CPOINT(I)
                  IWDOCO=-1
                  GOTO 10
               ELSE
                  GOTO 21
               ENDIF
            ENDIF
         ENDDO
         IF(ENDED)THEN
            IW1=IWORD
            GOTO 90
         ENDIF
       ENDIF
      ENDIF
 
*     is the pointer bit coded in cont card?
      IF(INDEX(CVALSA,'.').NE.0)THEN
         IF(Q(KQS+L+IWORD).GT.0. .AND. Q(KQS+L+IWORD).LT.1000000.)THEN
            IBVAL=Q(KQS+L+IWORD)
         ELSE
            IBVAL=0
         ENDIF
      ELSE
         IBVAL=IQ(KQS+L+IWORD)
      ENDIF
      IF(.NOT.NEWVAL)THEN
         IF(CVAR1(2:4).EQ.':BI')THEN
            READ(CVAR1(5:6),'(I2)',ERR=40  )I1BIT
            IF(I1BIT.GT.31)GOTO 24
            READ(CVAR1(7:8),'(I2)',ERR=24  )I2BIT
            IF(I2BIT.GT.31)GOTO 24
            IF(I2BIT.LT.I1BIT)GOTO 24
            IBVAL = JBYT(IQ(KQS+L+IWORD),I1BIT+1,I2BIT-I1BIT+1)
            IF(INDEX(CHOPT,'Z').NE.0)THEN
               WRITE(CVAL,'(Z11)')IBVAL
            ELSE
               WRITE(CVAL,'(I11)')IBVAL
            ENDIF
*           get the name
            IF(NW.LT.3)THEN
               CTEMP='XXXXXX'
            ELSE
               NCH=MIN(NW-2,2)*4
               IF(NCH.GT.6)NCH=6
               CALL UHTOC(IQ(IP+3),4,CTEMP,NCH)
*              embedded space
               IPSP=INDEX(CTEMP,' ')
               IF(IPSP.NE.0)CTEMP(IPSP:)=' '
            ENDIF
            CVAR1(3:8)=CTEMP
         ENDIF
      ENDIF
 
24    CONTINUE
 
*     if it is a pointer or rep count of it, remember its value
 
      IF(CVAR1(1:2).EQ.'P:' .OR. CVAR1(1:2).EQ.'N:')THEN
*        already there?
         IF(IPOINT.GT.0)THEN
            DO I=1,IPOINT
               IF(CPOINT(I).EQ.CVAR1)THEN
*                  IF(CVAR1(1:2).EQ.'P:')THEN
*                     IF(POINT(I).GE.0)THEN
*                        IF(POINT(I) .LT. IQ(KQS+L+IWORD))
*     &                  POINT(I)=IQ(KQS+L+IWORD)
*                        WRITE(*,*)'Ambigous pointer: ', CPOINT(I)
*                     ELSE
*                        POINT(I)=IBVAL
*                        GOTO  25
*                     ENDIF
*                  ELSE
                  IF(CVAR1(1:2).EQ.'N:')THEN
                     IF(NPOINT(I).GE.0)THEN
                        WRITE(*,*)'Ambigous rep count: ', CPOINT(I)
                     ELSE
*                       may be a simple expression of the value
                        CALL DZDGRC(IQ(IP),2,IBVAL,IREPC,NEXTRA,
     &                  IDUMMY,0,NDUMMY)
                        IF(IREPC.GT.0)IBVAL=IREPC
                        NPOINT(I)=IBVAL
                        GOTO  25
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
         IPOINT=IPOINT+1
         IF(IPOINT.GT.MAXP)THEN
            WRITE(*,*)'Too many pointers:',IPOINT
         ELSE
            CPOINT(IPOINT)=CVAR1
            IF(CVAR1(1:2).EQ.'P:')THEN
               POINT (IPOINT)=IBVAL
*              does it point to same as previous
               IF(IPOINT.GT.1)THEN
                  DO I=1,IPOINT-1
                     IF(POINT(I).EQ.POINT(IPOINT)
     &               .AND. POINT(I).NE.0)THEN
                        POINT(I)=-1
                       WRITE(*,*)'Pointer: ',CPOINT(I),
     &                  ' overwritten by: ',CPOINT(IPOINT)
                     ENDIF
                  ENDDO
               ENDIF
            ELSE
*              may be a simple expression of the value
               CALL DZDGRC(IQ(IP),2,IBVAL,IREPC,NEXTRA,
     &         IDUMMY,0,NDUMMY)
               IF(IREPC.GT.0)IBVAL=IREPC
               NPOINT (IPOINT)=IBVAL
            ENDIF
         ENDIF
25       CONTINUE
      ENDIF
 
26    CONTINUE
 
*     pointer stuff end
 
*       if cont line then look if its mask, bitvalue or choice
         LENMSK=0
         IF(.NOT.NEWVAL)THEN
*            wildchar mask
             IF(CVAR(1:8) .EQ. 'WILDCHAR' .OR.
     &          CVAR(1:4) .EQ. 'MASK')THEN
                IF(KEEPSQ)GOTO 80
*               get the mask
                IF(NW.LT.3)THEN
*                  no mask provided
                   GOTO 80
                ELSE
                   CVAR1 = ' '
                   CTEMP=' '
                   NCH=MIN(NW-2,2)*4
                   IF(NCH.GT.11)NCH=11
                   CALL UHTOC(IQ(IP+3),4,CTEMP,NCH)
*                  embedded space
                   NCH=INDEX(CTEMP,' ')-1
                   IF(NCH.LE.0)NCH=11
                   LENMSK=NCH
                   CTEMP(NCH+1:)=' '
*                  look if it matches
                   DO 28 I=11,1,-1
*                     mask ended?
                      IF(NCH.LE.0)THEN
                         IF(CVALSA(I:I).EQ. ' ')THEN
                            GOTO 29
                         ELSE
                            GOTO 80
                         ENDIF
                      ENDIF
*                     value ended, is rest of mask *?
                      IF(CVALSA(I:I).EQ. ' ')THEN
                         DO 27 J=NCH,1,-1
                           IF(CTEMP(J:J).NE.'*')GOTO 80
27                       CONTINUE
                         GOTO 29
                      ENDIF
                      IF(CTEMP(NCH:NCH).EQ.'*')GOTO 28
                      IF(CTEMP(NCH:NCH).NE.CVALSA(I:I))GOTO 80
28                 NCH=NCH-1
29                 CONTINUE
*                  all matched, does it still fit on line?
                   NCH=(NW-2)*4
                   IF(NCH+LALILE-LENMSK.GT.80)GOTO 50
 
                   CLINE=' '
                   CALL UHTOC(IQ(IP+3),4,CLINE,NCH)
                   NCH=LENOCC(CLINE)
                   LALILE=LALILE+NCH-LENMSK
                   WRITE(LUC,'(A'//NONEWL//')')CLINE(LENMSK+1:NCH)
                   LENMSK=0
                   GOTO 80
                ENDIF
            ENDIF
 
*            choice
             IF(CVAR(1:6) .EQ. 'BITVAL')THEN
                 IF(KEEPSQ)GOTO 80
                 READ(CVAR(7:8),'(I2)')IBIT
                 IF(IAND(IQ(KQS+L+IWORD),ISHFT(1,IBIT)).EQ.0)THEN
                     GOTO 80
                 ELSE
                     CVAR1 = ' '
                     GOTO 50
                 ENDIF
             ENDIF
*            choice starts with C, and
*            contains only numbers or  _  + -
*
             IF(CVAR(1:1).EQ.'C')THEN
                 ILC = INDXBC(CVAR,' ')
                 IF(ILC.LE.1)GOTO 40
                 IFNUM = 2
                 DO 30   K=2,ILC
                     IF(INDXNC(CVAR(K:K)) .NE. 0
     +               .AND.      CVAR(K:K) .NE. '_'
     +               .AND.      CVAR(K:K) .NE. '-'
     +               .AND.      CVAR(K:K) .NE. '+')GOTO 40
                     IF(CVAR(K:K) .EQ. '_')IFNUM=K+1
   30            CONTINUE
                 IF(IFNUM.GT.ILC)GOTO 40
                 IF(KEEPSQ)GOTO 80
                 READ(CVAR(IFNUM:ILC),'(I8)')IVAL
                 IF(INDEX(CVALSA,'.').NE.0)THEN
                    IBVAL=Q(KQS+L+IWORD)
                 ELSE
                    IBVAL=IQ(KQS+L+IWORD)
                 ENDIF
                 IF(IBVAL .NE. IVAL)THEN
                     GOTO 80
                 ELSE
                     CVAR1 = ' '
                     GOTO 50
                 ENDIF
             ENDIF
             IF(CVAR(1:4).EQ.'BITS')THEN
                 READ(CVAR(5:6),'(I2)',ERR=40  )I1BIT
                 IF(I1BIT.GT.31)GOTO 40
                 READ(CVAR(7:8),'(I2)',ERR=40  )I2BIT
                 IF(I2BIT.GT.31)GOTO 40
                 IF(I2BIT.LT.I1BIT)GOTO 40
                 IBVAL = JBYT(IQ(KQS+L+IWORD),I1BIT+1,I2BIT-I1BIT+1)
                 IF(INDEX(CHOPT,'Z').NE.0)THEN
                     WRITE(CVAL,'(Z11)')IBVAL
                 ELSE
                     WRITE(CVAL,'(I11)')IBVAL
                 ENDIF
             ENDIF
         ENDIF
   40    CONTINUE
*
   50    CONTINUE
         IF(IWORD .LT. IF11)GOTO 80
         IF(CVAR .EQ. 'UNDEFIND' .AND. INDEX(CHOPT,'U').NE. 0)GOTO 80
         IF(KEEPSQ)THEN
*           generate FORTRAN code
            IF(NEWVAL .AND. CVAR.NE.'UNDEFIND')THEN
               ILC = INDXBC(CVAR,' ')
*              suppress D:, P: etc
               IFNCOL=MAX(1,INDEX(CVAR(1:ILC),':')+1)
*              get IO-char
               NCH=MIN((NW-2)*4,60)
               CLINE=' '
               CALL UHTOC(IQ(IP+3),4,CLINE,NCH)
               ICOLON=INDEX(CLINE,'IO:')
               IF(ICOLON.NE.0)THEN
*                 skip space
                  ICOLON=ICOLON+2+INDEXC(CLINE(ICOLON:),' ')
                  CHARIO=CLINE(ICOLON:ICOLON)
                  IF(CHARIO.EQ.'U')CHARIO='B'
                  IF(CHARIO.EQ.'R')CHARIO='F'
                  IOLCHA=ICOLON+INDEX(CLINE(ICOLON:),' ')-1
               ENDIF
               IF(INDEX(CHOPT,'I').NE.0)THEN
                  IF(ICOLON.NE.0)THEN
                     IF(INDEX(CHARIA,CHARIO).EQ.0)THEN
                        WRITE(*,*)'Illegal IO-char: ',CHARIO
                        CHPF=' '
                        GOTO 995
                     ENDIF
                     IF(CHARIO.EQ.CHARIS)THEN
                        NSTYP=NSTYP+1
                     ELSE
                        IF(NSTYP.NE.0)THEN
                           CALL DZDFIO(CHPF,IPCHPF,NSTYP,CHARIS)
                           IF(IPCHPF.LE.0)GOTO 995
                        ENDIF
                        NSTYP=1
                     ENDIF
                  ELSE
                     IF(NSTYP.GT.0)THEN
                        NSTYP=NSTYP+1
                     ELSE
                        WRITE(*,*)'No IO-char given yet'
                     ENDIF
                  ENDIF
*                 end of get IO-char
               ELSE
*                 if parameter statements required force Integers
                  IF(INDEX(CHOPT,'p').NE.0) CHARIO='I'
*                 data word offsets, assignments
                  ISKC=INDEXN(CHOPT)
*                 skip first ISKCC characters in data word name
                  IF(ISKC.NE.0)THEN
                     CALL DZDCTI(CHOPT(ISKC:ISKC),ISKCC)
                     IFNCOL=MIN(IFNCOL+ISKCC,ILC)
                  ENDIF
*                 prepare pre or postfix
                  ILPF = MIN(LNBLNK(CHPF),LEN(CHPFI))
                  IF(ILPF.GT.0)THEN
                     CHPFI(1:ILPF)=CHPF(1:ILPF)
                  ELSE
                     IF(INDEX(CHOPT,'=').NE.0)THEN
                        IF(IREPL.GT.0)THEN
                           CHPFI='(I)= Q(NOFF+'
                           IF(CHARIO.EQ.'I')CHPFI(5:5)='I'
                           ILPF=12
                        ELSE
                           CHPFI='= Q(NOFF+'
                           IF(CHARIO.EQ.'I')CHPFI(2:2)='I'
                           ILPF=9
                        ENDIF
                     ELSE IF((INDEX(CHOPT,'C').NE.0 .OR.
     +               INDEX(CHOPT,'H').NE.0)
     +               .AND.IREPL.GT.0)THEN
                        IF(CHREPC.EQ.' ')THEN
                           CHPFI='(MW)'
                        ELSE
                           CHPFI='('//CHREPC(1:2)//')'
                        ENDIF
                        ILPF=4
                     ENDIF
                  ENDIF
                  IF(ILPF.GT.0)THEN
                     IF(INDEX(CHOPT,'Z').EQ.0 .AND.
     +                  INDEX(CHOPT,'=').EQ.0 .AND.
     +                  INDEX(CHPFI,'(').EQ.0)THEN
                        CVARNM(1:ILPF)=CHPFI(1:ILPF)
                        CVARNM(ILPF+1:)=CVAR(IFNCOL:ILC)
                     ELSE
                        CVARNM(1:ILC-IFNCOL+1)=CVAR(IFNCOL:ILC)
                        CVARNM(ILC-IFNCOL+2:)=CHPFI(1:ILPF)
                     ENDIF
                     ILC=ILC-IFNCOL+1+ILPF
                  ELSE
                     CVARNM=CVAR(IFNCOL:ILC)
                     ILC=ILC-IFNCOL+1
                  ENDIF
                  WRITE(CVAR1,'(I8)')IWORDR
                  IFC = INDEXN(CVAR1)
                  IF(INDEX(CHOPT,'=').NE.0)THEN
*                    data assignments
                     IF(NKEEPS.EQ.0)THEN
                        ILC1 = INDXBC(CHBANK(1:4),' ')
                        IF(IPRKEE.LE.0)THEN
                           IPRKEE=IPRKEE+1
                           WRITE(LUC,'(A)')
     +                    'C +KEEP,'//'DAASS'//CHBANK(1:ILC1)
                        ENDIF
                     ENDIF
                     CALL DZDPLN(LUC,
     +               CVARNM(1:ILC)//CVAR1(IFC:8)//')',2)
                  ELSE IF(INDEX(CHOPT,'D').NE.0)THEN
*                    declarations, commons
                     IF(NKEEPS.EQ.0)THEN
                        ILC1 = INDXBC(CHBANK(1:4),' ')
                        IF(IPRKEE.LE.0)THEN
                           IPRKEE=IPRKEE+1
                           WRITE(LUC,'(A)')
     +                     'C +KEEP,'//'DAOFF'//CHBANK(1:ILC1)
                        ENDIF
                        IF(INDEX(CHOPT,'C').NE.0)THEN
*                          commons
                           IF(CHPFI(1:1).EQ.'(' .AND.
     +                     CHPFI(ILPF:ILPF).EQ.')')THEN
*                             parameter statement for dimension
                              CALL DZDPLN(LUC,'INTEGER ',2)
                              CHPFI(1:1)=' '
                              CALL DZDPLN(LUC,CHPFI(1:ILPF-1),1)
                              CHPFI(1:1)='('
                              CALL DZDPLN(LUC,'PARAMETER (',2)
                              CALL DZDPLN(LUC,CHPFI(2:ILPF-1)//'=',1)
                              IF(IUPERC.GT.0)THEN
                                 WRITE(CTEMP,'(I12)')IUPERC
                                 IFC=INDEXC(CTEMP,' ')
                                 CALL DZDPLN(LUC,CTEMP(IFC:)//')',1)
                              ELSE
                                 CALL DZDPLN(LUC,'? )',1)
                              ENDIF
                           ENDIF
                           CALL DZDPLN(LUC,
     +                     'COMMON/CO'//CHBANK(1:ILC1)//'/',2)
                           CALL DZDPLN(LUC,
     +                     ' '//CVARNM(1:ILC),1)
                           ILC=0
                        ENDIF
                        IF(INDEX(CHOPT,'H').NE.0)THEN
*                          hbname
                           CALL DZDPLN(LUC,
     +                     'CALL HBNAME (ID'//CHBANK(1:ILC1)//
     +                     ','''//CHBANK(1:ILC1)//''',',2)
*                          if its an array fill dimension first
                           IF(CHPFI(1:1).EQ.'(' .AND.
     +                     CHPFI(ILPF:ILPF).EQ.')')THEN
                              CALL DZDPLN(LUC,
     +                        CHPFI(2:ILPF-1)//','''//CHPFI(2:ILPF-1),1)
                              IF(IUPERC.GT.0)THEN
                                 WRITE(CTEMP,'(I12)')ILOWRC
                                 IFC=INDEXC(CTEMP,' ')
                                 CALL DZDPLN(LUC,'['//CTEMP(IFC:),1)
                                 WRITE(CTEMP,'(I12)')IUPERC
                                 IFC=INDEXC(CTEMP,' ')
                                 CALL DZDPLN(LUC,
     +                           ','//CTEMP(IFC:)//']',1)
                              ELSE
                                 CALL DZDPLN(LUC,'[0,??]',1)
                              ENDIF
                           ELSE
                              CALL DZDPLN(LUC,
     +                        CVARNM(1:ILC)//',''',1)
                              CALL DZDPLN(LUC,CVARNM(1:ILC),1)
                              ILC=0
                           ENDIF
                        ENDIF
                     ENDIF
                     IF(INDEX(CHOPT,'C').EQ.0 .AND.
     +                  INDEX(CHOPT,'H').EQ.0)THEN
                        IF(CHARIO.NE.CHARIS)THEN
                           IF(CHARIO.EQ.'F')THEN
                              CALL DZDPLN(LUC,'REAL ',2)
                           ELSE
                              CALL DZDPLN(LUC,'INTEGER ',2)
                           ENDIF
                           CALL DZDPLN(LUC,
     +                     ' '//CVARNM(1:ILC),1)
                           ILC=0
                        ENDIF
                     ENDIF
                     IF(ILC.GT.0)THEN
                        CALL DZDPLN(LUC,
     +                  ','//CVARNM(1:ILC),1)
                     ENDIF
                     IF(INDEX(CHOPT,'H').NE.0)THEN
                        IF(ICOLON.GT.1)THEN
                           CALL DZDPLN(LUC,':'//CLINE(ICOLON:IOLCHA),1)
                        ENDIF
                     ENDIF
                  ELSE
*                    PARAMETER statements
                     IF(NKEEPS.EQ.0)THEN
                        CALL DZDPLN(LUC,'PARAMETER(',2)
                     ELSE
                        CALL DZDPLN(LUC,',',1)
                     ENDIF
                     CALL DZDPLN(LUC,
     +               CVARNM(1:ILC)//'='//CVAR1(IFC:8)
     +               ,1)
                  ENDIF
                  NKEEPS = NKEEPS+1
                  CHARIS=CHARIO
               ENDIF
            ENDIF
         ELSE
            MC1 = MIN(NW,17)
            MC=MC1
            DO 60   K=3,MC1
               IF(IQ(IP+K).EQ.IILFLF)THEN
                 MC=K-1
                 GOTO 70
               ENDIF
   60       CONTINUE
   70       CONTINUE
*           examine/check value against range
            IF(INDEX(CHOPT,'E').NE.0)THEN
*              get [ , ]
               NCH=MIN((NW-2)*4,LEN(CLINE))
               CLINE=' '
               CALL UHTOC(IQ(IP+3),4,CLINE,NCH)
               IFC=INDEX(CLINE,'[')
               ILC=INDEX(CLINE,']')
               IFC=IFC+1
               ILC=ILC-1
               NC=INDEX(CLINE(IFC:ILC),',')
               IF(NC.EQ.0)NC=INDEX(CLINE(IFC:ILC),':')
               IF(IFC.EQ.1 .OR. ILC.LE.0 .OR. NC.EQ.0)THEN
                  WRITE(LUC,'(A,I6)')
     +            'No range defined for word:',IWORD
                   GOTO 80
               ENDIF
               ILC1=IFC+NC-2
               IFC2=ILC1+2
               IOUT=0
               IF(INDEX( CVAL,'.').EQ.0)THEN
*                 INTEGER
                  IF(CLINE(IFC:IFC).NE.'*')THEN
                     CALL DZDCTI(CLINE(IFC:ILC1),IVAL1)
                     IF(IQ(KQS+L+IWORD).LT.IVAL1)THEN
                        IOUT=1
                     ELSE IF(CLINE(IFC2:IFC2).NE.'*')THEN
                        CALL DZDCTI(CLINE(IFC2:ILC),IVAL2)
                        IF(IQ(KQS+L+IWORD).GT.IVAL2)IOUT=1
                     ENDIF
                  ENDIF
                  IF(IOUT.GT.0)THEN
                     WRITE(LUC,'(A,I10,A,I6,A,2(I10,A))')
     +               CVAR1,IQ(KQS+L+IWORD),' at:',IWORD,
     +               ' out of range [', IVAL1,',' ,IVAL2,']'
                  ENDIF
               ELSE
                  IF(CLINE(IFC:IFC).NE.'*')THEN
                     CALL IZCTOR(CLINE(IFC:ILC1),VAL1)
                     IF(Q(KQS+L+IWORD).LT.VAL1)THEN
                        IOUT=1
                     ELSE IF(CLINE(IFC2:IFC2).NE.'*')THEN
                        CALL IZCTOR(CLINE(IFC2:ILC),VAL2)
                        IF(Q(KQS+L+IWORD).GT.VAL2)IOUT=1
                     ENDIF
                  ENDIF
                  IF(IOUT.GT.0)THEN
                     WRITE(LUC,'(A,E10.4,A,I6,A,2(E10.4,A))')
     +               CVAR1,Q(KQS+L+IWORD),' at:',IWORD,
     +               ' out of range [', VAL1,',' ,VAL2,']'
                  ENDIF
               ENDIF
*           print the value
            ELSE IF(INDEX(CHOPT,'=').EQ.0)THEN
*               fill in a possible index
                CLOOP=CVAR1
                IF(LOOPC1.GT.0)THEN
                   CLOOP(9:9)='('
                   CLOOP(21:21)=')'
                   WRITE(CLOOP(10:14),'(I5)')LOOPC1
                   IF(LOOPC2.GT.0)THEN
                      CLOOP(15:15)=','
                      WRITE(CLOOP(16:20),'(I5)')LOOPC2
                   ENDIF
                   CLOOP=SPACES(CLOOP,0)
                ENDIF
*               indent
                NCLOOP=LENOCC(CLOOP)
                IF(NCLOOP.LE.12)THEN
                     NCLOOP=12
                ELSE IF(NCLOOP.LE.16)THEN
                     NCLOOP=16
                ENDIF
                CLINE=' '
                WRITE(CLINE,'(1X,I5)')IWORD
 
                CLINE(9:9+NCLOOP-1)=CLOOP(1:NCLOOP)
                CLINE(9+NCLOOP:9+NCLOOP+10)=CVAL
                MC=(MC-2)*4
                MC=MIN(MC,LEN(CLINE)-21-NCLOOP)
                CALL UHTOC(IQ(IP+3),4,CLINE(22+NCLOOP-LENMSK:),MC)
*               remove leading part if its MASK
                IF(LENMSK.GT.0)CLINE(1:22+NCLOOP-1)=' '
                IF(CLINE (9+NCLOOP-1:MC+35).NE.
     &             CLINSA(9+NCLOOP-1:MC+35))THEN
                   IF(NLSKIP.GT.0)THEN
                      WRITE(LUC,'(/26X,A,I5,A)')'---',NLSKIP,
     &                ' lines with same value skipped ---'
*                      IF(IDELAY.LE.0)WRITE(LUC,'(A)')' '
                      NLSKIP=0
                   ENDIF
                   IF(IDELAY.GT.0)THEN
                      WRITE(LUC,'(/11X,A)')CLINDE
                      IDELAY=IDELAY-1
                   ENDIF
                   LALILE=LENOCC(CLINE)
                   WRITE(LUC,'(/A'//NONEWL//')')CLINE(1:LALILE)
*             only if bank has more then 100 words
                   IF(NBLENG.GT.100)CLINSA(1:MC+35)=CLINE(1:MC+35)
                ELSE
                   NLSKIP=NLSKIP+1
                ENDIF
             ELSE
                IF(INDEX( CVAL,'.').EQ.0)THEN
                   WRITE(LUC,'(6X,A,A4,A,A8,A,A11)')
     +             'IQ(L',CHBANK(1:4), '+', CVAR1, ') = ', CVAL
                ELSE
                   WRITE(LUC,'(6X,A,A4,A,A8,A,A11)')
     +             'Q(L',CHBANK(1:4), '+', CVAR1, ') = ', CVAL
                ENDIF
             ENDIF
         ENDIF
         GOTO 80
      ENDIF
 
*     end of hollerith sector
 
      IF(IT .NE. 2)THEN
         WRITE(LUC,'(/A,2I9)')' Unexpected IT,NW ',IT,NW
         WRITE(LUC,'(1X,100Z8)')(IQ(IP+K),K=1,NW)
         GOTO 80
      ENDIF
 
*     start of integer sector
*     i.e. word # documented or rep level change
 
*     WRITE(*,'(1X,Z8,100I8)')(IQ(IP+K),K=1,NW)
      ICHOIC = IQ(IP+1)
*      IBIT13 = JBYT(ICHOIC,1,3)
      IDENTF = JBYT(ICHOIC,11,5)
      IF(KEEPSQ )THEN
         IF(IREPL.NE.JBYT(ICHOIC,16,3))THEN
            IF(INDEX(CHOPT,'C').NE.0)NKEEPS=0
            IF(INDEX(CHOPT,'H').NE.0)THEN
               IF(NKEEPS.GT.0)CALL DZDPLN(LUC,''')',1)
               NKEEPS=0
            ENDIF
         ENDIF
      ENDIF
 
      IREPL=JBYT(ICHOIC,16,3)
*      IESEQ  = JBYT(ICHOIC,19,1)
*      WRITE(*,*)' IBIT13, IDENTF, IREPL, IESEQ',
*     &               IBIT13, IDENTF, IREPL, IESEQ
*     remember sequence number of data word documented
      IWDOCO = IWDOC
      IF(IDENTF .EQ. ITGENQ)THEN
*        remember start of rep section in doc
         IF(IC1.GT.0 .AND. IP1 .EQ. 0)IP1 =IP
         IF(IC2.GT.0 .AND. IP2 .EQ. 0)IP2 =IP
         IWDOC = IQ(IP+2)
         GOTO 80
      ENDIF
 
*     repetition level
 
      IF(IDENTF .EQ. ITGREQ)THEN
          IF(REPEND)THEN
             REPEND=.FALSE.
          ELSE
             IF(KEEPSQ)IWORDR=0
          ENDIF
*         end of infinite rep loop (IFREPL=1), check if doc is uptodate
 
          IF(IFREPL.NE.0 .AND. NEXTRA.EQ.0 .AND. LRECOV
     &     .AND. IMBED2.EQ.0 .AND. NW.EQ.1 .AND. IREPL.EQ.0)THEN
             NTRIAL=0
             NDREST=IQ(L+KQS-1)-IWORIF
             NNREP=FLOAT(NDREST)/FLOAT(IWDOC)+0.5
74           NDEXT=NDREST-IWDOC*NNREP
             IF(NDEXT.NE.0 .AND. NNREP.GT.0)THEN
                NEXTRA=NDEXT/NNREP
                IF(NNREP*NEXTRA.EQ.NDEXT)THEN
                   IF(INDEX(CHOPT,'Q').EQ.0)
     &             CALL DZDPRW(LUC,NEXTRA+IWDOC)
                ELSE
                   IF(NTRIAL.EQ.0)THEN
                      NNREP=NNREP+1
                      NTRIAL=1
                      GOTO 74
                   ELSEIF(NTRIAL.EQ.1)THEN
                      NNREP=NNREP-2
                      NTRIAL=-1
                      GOTO 74
                   ELSE
                      NEXTRA=0
                   ENDIF
                ENDIF
             ELSE
                NEXTRA=0
             ENDIF
             IF(NEXTRA.LT.0)THEN
                 WRITE(LUC,'(/A,I5,A)')
     &           ' Ignore last',-NEXTRA,' words'
                 IWORD=IWORD+NEXTRA
                 IWORDR=IWORDR+NEXTRA
*                force new retrieval of formatted values
                 IC9=10
                 NEXTRA=-(NEXTRA+IWDOC)
             ENDIF
             IF(NEXTRA.EQ.0 .AND. NDEXT.NE.0)THEN
                   CALL DZDPRW(LUC,-1000000)
             ENDIF
          ENDIF
*     look if enough data words are documented for this section
          IF(NEXTRA.GT.0 .AND. LRECOV)THEN
             DO 75 II=IWORD+1,IWORD+NEXTRA
                CALL DZSHOW(' ',IXSTOR, L,COPTDZ//'Q',1,0,II,II)
                CVAL = CQLINE(12:22)
                IF(INDEX(CHOPT,'C').EQ.0)THEN
                   IF(INDEX(CHOPT,'Q').EQ.0)
     &             WRITE(LUC,'(/1X,I5,16X,A11,A)') II, CVAL,
     &            '  not documented'
                ELSE
                   WRITE(LUC,'(A11)')CVAL
                ENDIF
   75        CONTINUE
             IWORD=IWORD+NEXTRA
             IWORDR=IWORDR+NEXTRA
*            force new retrieval of formatted values
             IC9=10
          ENDIF
 
*
76        IWDOC = -1
 
*     end of rep level
 
          IF(NW .EQ. 1)THEN
 
             IF(IREPL .EQ. 1)THEN
*               end rep lev 2
                IF(REPATH)THEN
                   IF(IP.NE.IPDRE1)THEN
                      IF(IPRL1.LE.0)THEN
                         NVAL=-1
                         GOTO 990
                      ENDIF
                      VVAR(IPRL2)=IWORD-IPREP1
                      IPRL2=0
                      IPDRE1=IP
                      IF(NOTFIL)THEN
                         NVAL=NVAL+1
                         VVAR(NVAL)=0
                      ENDIF
                   ENDIF
                   IPREP1=IWORD
                ENDIF
                IC2 = IC2 -1
                LOOPC2=LOOPC2+1
*                IREPC=-2
                IF(IC2 .GT. 0)THEN
                    IP = IP2
                    IDENTF = ITGENQ
                    GOTO 10
                ENDIF
                LOOPC2=-1
                NEXTRA=0
                GOTO 80
             ELSE
                IF(IREPL .EQ. 0)THEN
                   IF(REPATH)THEN
*                     all done
                      IF(IP.NE.IPDRE0)THEN
                         IF(IPRL1.LE.0)THEN
                            NVAL=-1
                            GOTO 990
                         ENDIF
                         VVAR(IPRL1)=IWORD-IPREP0
                         IPDRE0=IP
                         IPRL1=0
                         IF(NOTFIL)THEN
                            NVAL=NVAL+1
                            VVAR(NVAL)=0
                         ENDIF
                      ENDIF
                      IF(.NOT.NOTFIL)GOTO 990
                      IPREP0=IWORD
                   ENDIF
                   IC1=IC1-1
                   LOOPC1=LOOPC1+1
*                   IREPC=-2
                   IF(IC1 .GT. 0)THEN
                      IF(KEEPSQ)GOTO 990
                      IP = IP1
                      IDENTF = ITGENQ
                      GOTO 10
                   ENDIF
                   LOOPC1=-1
                   NEXTRA=0
                   IMBED2=0
                   GOTO 80
                ELSE
                   WRITE(LUC,'(/A,I9)')' Illegal end of rep at',IP
                   GOTO 990
                ENDIF
             ENDIF
          ENDIF
 
* start of rep
 
          IREPC = IQ(IP+2)
 
*         return path?
 
          IF(REPATH)THEN
             NVAL=NVAL+1
*            offset from previous to start of rep
             VVAR(NVAL)= - (IWORD-IPREP0)
*            remember start of section
             IF(IREPL.EQ.2)THEN
                IPREP1=IWORD
             ELSE
                IPREP0=IWORD
             ENDIF
             NVAL=NVAL+1
             VVAR(NVAL)=4
             NVAL=NVAL+1
             VVAR(NVAL)=IREPC
             MGEVAL=MAXVAL-NVAL
          ELSE
             MGEVAL=0
          ENDIF
 
          IF(IREPC.LT.0 .AND. .NOT. KEEPSQ)THEN
*            was it given in previous data word?
             ITP = MOD(IQ(IP+NW+1),16)
             NWP = IQ(IP+NW+1)/16
             NCOP=MIN(15,NWP)*4
             IF(ITP.EQ.5)THEN
                CLINE=' '
                CALL UHTOC(IQ(IP+NW+2),4,CLINE,NCOP)
                IF(CLINE(1:2).EQ.'N:'.AND.IPOINT.GT.0)THEN
                   DO I=1,IPOINT
                      NCH=LENOCC(CPOINT(I))
                      IF(CPOINT(I)(1:NCH).EQ.CLINE(1:NCH))THEN
                         IREPC=NPOINT(I)
                         IF(LENOCC(CLINE).GT.NCH)THEN
                            IVALRC=IREPC
                            IF(REPATH)THEN
                               CALL DZDGR1
     &                         (CLINE(NCH+1:),IVALRC,IREPC,NEX,
     &                          VVAR(NVAL+1),MGEVAL,NGEVAL)
                            ELSE
                               CALL DZDGR1
     &                         (CLINE(NCH+1:),IVALRC,IREPC,NEX,
     &                          IDUMMY,0,NDUMMY)
                            ENDIF
                         ENDIF
                         GOTO 77
                      ENDIF
                   ENDDO
                ENDIF
             ENDIF
 
*            try to get rep count from variable
 
             IF(REPATH)THEN
                CALL DZDGRC(IQ(IP+NW+1),0,IVALLW,IREPC,NEXTRA,
     &          VVAR(NVAL+1),MGEVAL,NGEVAL)
                IF(NGEVAL.LE.0)THEN
                   VVAR(NVAL)=1000000
                ELSE
                   VVAR(NVAL-1)=VVAR(NVAL-1)+NGEVAL
                   NVAL=NVAL+NGEVAL
                ENDIF
             ELSE
                CALL DZDGRC(IQ(IP+NW+1),0,IVALLW,IREPC,NEXTRA,
     &          IDUMMY,0,NDUMMY)
             ENDIF
 
             IF(NEXTRA.LT.0 .AND.INDEX(CHOPT,'Q').EQ.0 .AND. LRECOV)
     &       CALL DZDPRW(LUC,NEXTRA)
             IF(NEXTRA.GT.0 .AND. LRECOV)THEN
                IF(INDEX(CHOPT,'Q').EQ.0)WRITE(*,*)
     &          ' WARNING: Assume EXTRA',NEXTRA,' words not documented'
             ENDIF
 
             IF(IREPC.EQ.0)THEN
                REPEND=.TRUE.
             ENDIF
          ENDIF
77        CONTINUE
 
          IF(REPATH)THEN
             NVAL=NVAL+1
             IF(IREPL.EQ.1)THEN
                IPRL1=NVAL
             ELSE
                IPRL2=NVAL
             ENDIF
          ENDIF
 
          IF(IREPL.EQ.1)THEN
             IP1 = 0
             LOOPC1=1
             IC1 = IREPC
*             IC1 = IQ(IP+2)
             IF(IC1 .LT. 0)THEN
                IF(IFREPL .EQ. 0)THEN
                   IF(KEEPSQ)THEN
                      IC1 = 1
                   ELSE
*                    variable (infinite)rep
                      IFREPL =1
                      IC1 = 10000000
                   ENDIF
                   IDENTF = -ITGREQ
                   IF(NEWPNT)THEN
                      IWORIF=IWORD+1
                   ELSE
                      IWORIF=IWORD
                   ENDIF
                ELSE
                   WRITE(LUC,'(/A,I9)')
     &             ' Illegal var Rep count at ',IP
                   GOTO 990
                ENDIF
             ENDIF
             GOTO 80
          ELSE
             IF(IREPL.EQ.2)THEN
                IP2 = 0
                LOOPC2=1
                IC2 = IREPC
*               flag imbedded rep level 2
                IMBED2=1
*                IC2 = IQ(IP+2)
                IF(IC2 .LT. 0)THEN
                   WRITE(LUC,'(/A,I9)')
     &             ' Illegal var Rep count at ',IP
                   GOTO 990
                ENDIF
             ELSE
                WRITE(LUC,'(/A,I9)')' Repl>2 at ',IP
                GOTO 990
             ENDIF
          ENDIF
      ENDIF
 
   80 CONTINUE
      IP = IP+NW+1
      IF(IWORD.LT.IL11 .OR. IP.LT.LDATUM)THEN
          GOTO 10
      ELSE
          GOTO 990
      ENDIF
 
   90 CONTINUE
      IF(NLSKIP.GT.0)THEN
         WRITE(LUC,'(/A,I5,A)')'       ---',NLSKIP,
     &   ' identical lines skipped ---'
         NLSKIP=0
      ELSE
         WRITE(LUC,'(/A)')' '
      ENDIF
      DO 100 IWORD=IW1,IL11
      CALL DZSHOW(' ',IXSTOR, L,COPTDZ//'Q',1,0,IWORD,IWORD)
      CVAL = CQLINE(12:22)
      IF(INDEX(CHOPT,'C').EQ.0)THEN
          WRITE(LUC,'(1X,I5,16X,A11)') IWORD, CVAL
      ELSE
          WRITE(LUC,'(A11)')CVAL
      ENDIF
  100 CONTINUE
 
  990 CONTINUE
      IF( .NOT. KEEPSQ)THEN
         IF(NLSKIP.GT.0)THEN
            WRITE(LUC,'(/A,I5,A)')'       ---',NLSKIP,
     &      ' identical lines skipped ---'
            NLSKIP=0
         ELSE
            WRITE(LUC,'(/A)')
     &     '-----------------------------------------------------------'
         ENDIF
      ENDIF
  995 CONTINUE
      IF(INDEX(CHOPT,'I').NE.0 .AND. NSTYP.GT.0)THEN
          CALL DZDFIO(CHPF,IPCHPF,NSTYP,CHARIS)
      ENDIF
      IF(KEEPSQ .AND. INDEX(CHOPT,'H').NE.0 .AND. NKEEPS.GT.0)THEN
         CALL DZDPLN(LUC,''')',1)
      ENDIF
      IF(LQBKD1.NE.0 .AND.INDEX(CHOPT,'I').EQ.0)THEN
         CALL MZDROP(0,LQBKD1,'L')
         LQBKD1=0
      ENDIF
      IF (IENTRY.EQ.2) ITYYY=ITY
      IQPRNT =LUNSAV
      END
*********************************************************************
