*
* $Id: fmcopc.F,v 1.1.1.1 1996/03/07 15:17:42 mclareni Exp $
*
* $Log: fmcopc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:42  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMCOPC
#include "fatmen/fmpath.inc"
#include "fatmen/fatran.inc"
#include "fatmen/fatsat.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatoptd.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatusr.inc"
      CHARACTER*255 GENAM,GNAME,GENAM2
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS1(LKEYFA),KEYS2(LKEYFA)
      CHARACTER*15   VID
      CHARACTER*6    VSN
      CHARACTER*255  FILE
      CHARACTER*8    NODE
      CHARACTER*8    POOL
      CHARACTER*8    LIBRARY
      CHARACTER*8    CHOPT,CHOPC
      CHARACTER*8    MEDIA,DENS,COMPACT
      CHARACTER*8    CHHOST,CHTYPE,CHSYS
      CHARACTER*8    CHTRANS
      INTEGER        FMHOST,FMNODE
      INTEGER        FSEQ
      SAVE           CHHOST,CHTYPE,CHSYS,NENTRY
      SAVE           LHOST ,LTYPE ,LSYS
      DATA           MEDIA/'3480'/,DENS/'38K'/,COMPACT/' '/
      DATA           NENTRY/0/
#include "fatmen/fatinit.inc"
 
      IF(NENTRY.EQ.0) THEN
         IC     = FMHOST(CHHOST,CHTYPE,CHSYS)
         CALL CLTOU(CHHOST)
         CALL CLTOU(CHTYPE)
         CALL CLTOU(CHSYS)
         LHOST  = LENOCC(CHHOST)
         LTYPE  = LENOCC(CHTYPE)
         LSYS   = LENOCC(CHSYS)
         NENTRY = 1
      ENDIF
*
*     This routine performs copy functions in the following way:
*
*     COPY GENAM KS1 KS2 # both entries are assumed to be in the catalogue
*     COPY GENAM [KS1] POOL= LIBRARY = # copy entry to tape allocated from
*                                        named pool in named library
*     COPY GENAM [KS1] VID=  # copy entry to named VID (prefix.vid)
*     COPY GENAM [KS1] NODE= [FILE=] # copy entry to specified node
*
*     Options for FMALLO: MEDIA, DENS and COMPACT currently not supported
*     (nor multi-file option: M)
*
*     Save current directory
*
      CALL RZCDIR(CDIR,'R')
      LCDIR = LENOCC(CDIR)
*
*     Get command arguments
*
      GENAM = ' '
      KS1   = 0
      KS2   = 0
      POOL  = ' '
      NODE  = ' '
      FILE  = ' '
      VSN   = ' '
      VID   = ' '
      CHOPT = ' '
      LFILE = 0
      LNODE = 0
      CALL KUGETC(GENAM,LGN)
      CALL FMFIXF(GENAM,GNAME)
      LGN   = LENOCC(GNAME)
      GENAM(1:LGN) = GNAME(1:LGN)
      CALL KUGETI(KS1)
      CALL KUGETI(KS2)
      CALL KUGETC(POOL,LPOOL)
      CALL KUGETC(LIBRARY,LLIB)
      CALL KUGETC(VSN,LVSN)
      CALL KUGETC(VID,LVID)
      CALL KUGETI(FSEQ)
      CALL KUGETC(NODE,LNODE)
      CALL KUGETC(FILE,LFILE)
      CALL KUGETC(CHTRANS,LTRANS)
      CALL KUGETI(ILOC)
      CALL KUGETI(ICOP)
      CALL KUGETI(IMED)
      CALL KUGETC(CHOPT,LCH)
      IF(LCH.EQ.0) THEN
         CHOPT = ' '
         LCH   = 1
      ENDIF
 
 
      IF((LVSN.EQ.0).AND.(LVID.NE.0)) THEN
         VSN  = VID
         LVSN = LVID
      ENDIF
*
*     Parse options...
*
      CALL UOPTC(CHOPT,ALFNUM,IOPT)
*
      CHOPC = ' '
      LOPTC = 1
 
      IF(IOPTC.NE.0) THEN
         CHOPC(LOPTC:LOPTC) = 'C'
         LOPTC = LOPTC + 1
      ENDIF
*
      IF(IOPTS.NE.0) THEN
         CHOPC(LOPTC:LOPTC) = 'S'
         LOPTC = LOPTC + 1
      ENDIF
*
      IF(IOPTZ.NE.0) THEN
         CHOPC(LOPTC:LOPTC) = 'Z'
*        LOPTC = LOPTC + 1
      ENDIF
*
         CALL VZERO(KEYS1,10)
         CALL VZERO(KEYS2,10)
         KEYS1(1) = KS1
         KEYS2(1) = KS2
         LBANK1 = 0
         LADDBK = 0
*
*     Get input bank
*
         CALL FMGBYK(GENAM(1:LGN),LBANK1,KEYS1,IRET)
         IF(IRET.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMCOPC. error in FMGBYK'
            RETURN
         ENDIF
*
*     Lift bank for output file, or read from FATMEN catalogue
*
         IF(KS2.EQ.0) THEN
            CALL FMBOOK(GENAM(1:LGN),KEYS2,LADDBK,LSUP,2,IRET)
*
*     Copy file name
*
            CALL UCOPY(IQ(LBANK1+KOFUFA+MFQNFA),
     +                 IQ(LADDBK+KOFUFA+MFQNFA),NFQNFA/4)
*
*     Copy file area and creation date...
*
            CALL UCOPY(IQ(LBANK1+KOFUFA+MSRDFA),
     +                 IQ(LADDBK+KOFUFA+MSRDFA),MCRTFA-MSRDFA+1)
*
*     Copy system and user area...
*
            CALL UCOPY(IQ(LBANK1+KOFUFA+MFPRFA),
     +                 IQ(LADDBK+KOFUFA+MFPRFA),NWDSFA-MFPRFA+1)
*
*     Keys
*
            IF(ILOC.NE.0) THEN
               KEYS2(MKLCFA) = ILOC
               IQ(LADDBK+KOFUFA+MLOCFA) = ILOC
            ENDIF
 
            IF(ICOP.NE.0) THEN
               KEYS2(MKCLFA) = ICOP
               IQ(LADDBK+KOFUFA+MCPLFA) = ICOP
            ENDIF
 
            IF(IMED.NE.0) THEN
               KEYS2(MKMTFA) = IMED
               IQ(LADDBK+KOFUFA+MMTPFA) = IMED
            ENDIF
 
         ELSE
            CALL FMGBYK(GENAM(1:LGN),LADDBK,KEYS2,IRET)
            IF(IRET.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMCOPC. return code ',IRET,
     +         'from FMGBYK'
               RETURN
            ENDIF
         ENDIF
*
*     Now decide what we have to do...
*
         IF((LPOOL.NE.0).AND.(LLIB.NE.0)) THEN
*
*     Copy to tape from named pool
*
            IQ(LADDBK+MMTPFA+KOFUFA) = 2
            KEYS2(MKMTFA)            = 2
            IF(IDEBFA.GE.0) PRINT *,'FMCOPC. Copy to tape allocated '
     +      //'from ', 'pool ',POOL(1:LPOOL),' in library ',LIBRARY(1:
     +      LLIB)
*
*     Allocate new tape
*
            CALL FMALLO(MEDIA,DENS,COMPACT,LIBRARY(1:LLIB),POOL(1:
     +      LPOOL), LADDBK,' ',VSN,VID,IRET)
            IF(IRET.NE.0) THEN
               PRINT *,'FMCOPC. unable to allocate a new tape'
               GOTO 99
            ENDIF
         ELSEIF((LPOOL.NE.0).AND.(LLIB.EQ.0)) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCOPC. Library name must be '
     +      //'specified ', 'if pool is given.'
            GOTO 99
 
         ELSEIF((LPOOL.EQ.0).AND.(LLIB.NE.0)) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCOPC. Pool name must be '
     +      //'specified ', 'if library is given.'
            GOTO 99
 
         ELSEIF(LVID.NE.0) THEN
*
*     Copy to specified VID
*
            IF(IDEBFA.GE.0) PRINT *,'FMCOPC. Copy to tape ',VID(1:LVID)
*
*     Obtain tape details from TMS, add to FATMEN bank and then
*     call FMCOPY. VID maybe in extended VID format. In all
*     cases, it is also added to the bank (if LBANK/=0)
*
            IF(IMED.EQ.0) THEN
               IQ(LADDBK+MMTPFA+KOFUFA) = 2
               KEYS2(MKMTFA)            = 2
            ENDIF
            CALL UCTOH(VID,IQ(LADDBK+KOFUFA+MVIDFA),4,6)
            IQ(LADDBK+KOFUFA+MFSQFA) = FSEQ
            IF(LVSN.EQ.0) THEN
               CALL FMGTMS(VID,LADDBK,VSN,IDENS,IMEDIA,IRET)
            ELSE
               CALL UCTOH(VSN,IQ(LADDBK+KOFUFA+MVSNFA),4,6)
            ENDIF
 
         ELSEIF(LNODE.NE.0) THEN
*
*     Allow node=*
*
            IF(NODE(1:LNODE).EQ.'*') THEN
               NODE  = CHHOST(1:LHOST)
               LNODE = LHOST
            ENDIF
*
*     Copy to remote node
*
            IF(IDEBFA.GE.0) PRINT *,'FMCOPC. Copy to node ',NODE(1:
     +      LNODE)
*
*     Is the node specified local or remote?
*
            IREMOTE = FMNODE(NODE(1:LNODE))
 
            CALL VBLANK(IQ(LADDBK+KOFUFA+MHSNFA),NHSNFA/4)
            IF(IREMOTE.EQ.0) THEN
               CALL VBLANK(IQ(LADDBK+KOFUFA+MHSTFA),2)
               CALL UCTOH(CHTYPE,IQ(LADDBK+KOFUFA+MHSTFA),4,LTYPE)
               CALL UCTOH(CHSYS ,IQ(LADDBK+KOFUFA+MHOSFA),4,LSYS)
            ENDIF
            CALL UCTOH(NODE,IQ(LADDBK+KOFUFA+MHSNFA),4,LNODE)
            IF(KS2.EQ.0.AND.IMED.EQ.0) THEN
               IQ(LADDBK+MMTPFA+KOFUFA) = 1
               KEYS2(MKMTFA)            = 1
            ELSE
               IF(IDEBFA.GE.0) PRINT *,'FMCOPC. Copy from KS1 to KS2',
     +            KS1,KS2
            ENDIF
         ELSEIF(KS2.NE.0) THEN
*
*     Copy according to entries in FATMEN catalogue
*
            IF(IDEBFA.GE.0) PRINT *,'FMCOPC. Copy from KS1 to KS2',KS1,
     +      KS2
         ELSE
            PRINT *,'FMCOPC. Unknown copy options. You can copy to:'
            PRINT *,'- a tape allocated from a pool in a TMS library'
            PRINT *,'- a specified VID'
            PRINT *,'- a specified node (file) '
            PRINT *,'- the location indicated by an existing catalogue '
     +      //'entry'
            GOTO 99
         ENDIF
*
*     Set use count
*
         IQ(LADDBK+KOFUFA+MUSCFA) = 1
*
*     Fill in file name if specified
*
         IF(LFILE.NE.0) THEN
            CALL VBLANK(IQ(LADDBK+KOFUFA+MFQNFA),NFQNFA/4)
            CALL UCTOH(FILE,IQ(LADDBK+KOFUFA+MFQNFA),4,LFILE)
         ELSE
            IF((KEYS1(MKMTFA).NE.KEYS2(MKMTFA)).AND.
     +         (KEYS1(MKMTFA).EQ.1.OR.KEYS2(MKMTFA).EQ.1)) THEN
               PRINT *,'FMCOPC. Error - filename must be given when',
     +            ' copying from disk to tape or vice versa'
               GOTO 99
            ENDIF
         ENDIF
*
*     Verify input and output banks
*
         CALL FMVERI(GENAM(1:LGN),LBANK1,KEYS1,'A',IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMCOPC. errors in input bank'
            GOTO 99
         ENDIF
 
         CALL FMVERI(GENAM(1:LGN),LADDBK,KEYS2,'A',IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMCOPC. errors in output bank'
            GOTO 99
         ENDIF
*
*     Transport type
*
      ITRANS = 0
      IF(LTRANS.GT.0) THEN
         ITRANS = ICNTH(CHTRANS(1:LTRANS),CHTRNS,NTRANS)
         IF(ITRANS.EQ.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCOPC. unknown transport ',
     +         CHTRANS(1:LTRANS)
         ELSE
            IF(IDEBFA.GE.0) PRINT *,'FMCOPC. transport set to ',
     +         TRANSP(ITRANS)
            CHTRANS = TRANSP(ITRANS)
            LTRANS  = LENOCC(CHTRANS)
            IF(CHTRANS(1:LTRANS).EQ.'CHEOPS') THEN
               IF(LDSTST.EQ.0.OR.LSRCST.EQ.0) THEN
                  PRINT *,'FMCOPC. error - you must set the ',
     +               'source and destination sites'
                  PRINT *,'FMCOPC. use the commands SET/SOURCE',
     +               ' and SET/DESTINATION'
                  RETURN
               ENDIF
               LOPTC = LOPTC + 1
               CHOPC(LOPTC:LOPTC) = 'K'
            ENDIF
         ENDIF
      ENDIF
*
*     Now we know what to do - call the copy routine
*
         IF(IDEBFA.GE.3) CALL FMSHOW(GENAM(1:LGN),LADDBK,KEYS2,'A',
     +   IRET)
         IF(IDEBFA.GE.3) PRINT *,'FMCOPC. call FMCOPY for ',
     +      GENAM(1:LGN),' ',CHOPC(1:LOPTC)
 
         GENAM2 = GENAM(1:LGN)
         CALL FMCOPY(GENAM(1:LGN),LBANK1,KEYS1, GENAM2(1:LGN),LADDBK,
     +   KEYS2,CHOPC(1:LOPTC),IRC)
         IF(IRC.NE.0) THEN
            PRINT *,'FMCOPC. return code ',IRC,' from FMCOPY'
         ELSE
         IF(IOPTL.NE.0) CALL FMLOCK(GENAM(1:LGN),LBANK1,KEYS1,' ',IC)
         ENDIF
   99    CONTINUE
*
*     Reset current directory
*
         CALL RZCDIR(CDIR(1:LCDIR),' ')
      END
