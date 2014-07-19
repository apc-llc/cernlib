*
* $Id: fmplog.F,v 1.1.1.1 1996/03/07 15:18:20 mclareni Exp $
*
* $Log: fmplog.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:20  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPLOG(LUN,IHEAD,LHEAD,IRC)
#include "fatmen/fausto.inc"
      DIMENSION    IHEAD(LHEAD)
      DIMENSION    HEAD(KLREAL)
      CHARACTER*20 CHFSYS,CHFGRP,CHUSER,CHHOST,CHTYPE,CHSYS
      CHARACTER*64 CHTIT
*
*     Print contents of log record on LUN
*
      IRC = 0
*
*     Unpack hollerith
*
      CALL UHTOC(IHEAD(KFMSYS),4,CHFSYS,20)
      CALL UHTOC(IHEAD(KFMGRP),4,CHFGRP,20)
      CALL UHTOC(IHEAD(KFMTIT),4,CHTIT ,64)
      CALL UHTOC(IHEAD(KFMUSR),4,CHUSER,20)
      CALL UHTOC(IHEAD(KFMHST),4,CHHOST,20)
      CALL UHTOC(IHEAD(KFMTYP),4,CHTYPE,20)
      CALL UHTOC(IHEAD(KFMOS ),4,CHSYS ,20)
*
*     Unpack reals
*
      CALL UCOPY(IHEAD(KFMMBR),HEAD,KLREAL)
*
*     Unpack dates and times
*
      CALL FMUPTM(IDQ,ITQ,IHEAD(KFMIDQ),IRC)
      CALL FMUPTM(IDS,ITS,IHEAD(KFMIDS),IRC)
      CALL FMUPTM(IDE,ITE,IHEAD(KFMIDE),IRC)
 
      WRITE(LUN,9001) CHFSYS,CHFGRP,CHUSER,CHHOST,CHTYPE,CHSYS
 9001 FORMAT(' FMPLOG. FATMEN system and group = ',A,1X,A,/,
     +       '                            user = ',A,/,
     +       '   host name, type and system = ',A,1X,A,1X,A)
 
      WRITE(LUN,9002) CHTIT,IDQ,ITQ
 9002 FORMAT(' FMPLOG. FATMEN version is'/,1X,A,/,
     +       '         generated on ',I6,' at ',I4)
 
      WRITE(LUN,9003) IDS,ITS,IDE,ITE
 9003 FORMAT(' FMPLOG. start session at ',I6,1X,I4,/,
     +       '         end   session at ',I6,1X,I4/)
 
      WRITE(LUN,9004) (HEAD(J),J=1,7)
 9004 FORMAT(' FMPLOG. read             ',F10.2,' MB',/,
     +       '         wrote            ',F10.2,' MB',/,
     +       '         read  (FZ)       ',F10.2,' MB',/,
     +       '         wrote (FZ)       ',F10.2,' MB',/,
     +       '         copied (total)   ',F10.2,' MB',/,
     +       '         copied (network) ',F10.2,' MB',/,
     +       '         copied (queued)  ',F10.2,' MB',/)
 
      WRITE(LUN,9005)
 9005 FORMAT(' FMPLOG. catalogue manipulations:',/,1X,32('='),/)
 
      WRITE(LUN,9006) (IHEAD(J),J=KFMADD,KFMTCH)
 9006 FORMAT(' # Add disk = ',I6,/,
     +       ' # Add link = ',I6,/,
     +       ' # Add tape = ',I6,/,
     +       ' # MKDIR    = ',I6,/,
     +       ' # RMDIR    = ',I6,/,
     +       ' # RMLN     = ',I6,/,
     +       ' # RMTREE   = ',I6,/,
     +       ' # RM       = ',I6,/,
     +       ' # CP       = ',I6,/,
     +       ' # MV       = ',I6,/,
     +       ' # MODIFY   = ',I6,/,
     +       ' # TOUCH    = ',I6,/)
 
      WRITE(LUN,9007)
 9007 FORMAT(' FMPLOG. file accesses and copies:',/,1X,33('='),/)
 
      WRITE(LUN,9008) (IHEAD(J),J=KFMOPN,KFMCPN)
 9008 FORMAT(' # OPENs          = ',I6,/,
     +       ' # CLOSEs         = ',I6,/,
     +       ' # COPYs (total)  = ',I6,/,
     +       ' # COPYs (queued) = ',I6,/,
     +       ' # COPYs (remote) = ',I6,/)
 
      WRITE(LUN,9009)
 9009 FORMAT(' FMPLOG. TMS commands:',/,1X,21('='),/)
 
      WRITE(LUN,9010) (IHEAD(J),J=KFMSRQ,KFMSTG)
 9010 FORMAT(' # SYSREQs  = ',I6,/,
     +       ' # QVOLs    = ',I6,/,
     +       ' # Alloc VL = ',I6,/,
     +       ' # Alloc SP = ',I6,/,
     +       ' # POOLs    = ',I6,/,
     +       ' # LOCKs    = ',I6,/,
     +       ' # UNLOCKs  = ',I6,/,
     +       ' # DEL TAGS = ',I6,/,
     +       ' # GET TAGS = ',I6,/,
     +       ' # SET TAGS = ',I6,/)
 
      WRITE(LUN,9011)
 9011 FORMAT(' FMPLOG. FATMEN client commands:',/,1X,31('='),/)
 
      WRITE(LUN,9012) (IHEAD(J),J=KFMBNK,KFMMTC)
 9012 FORMAT(' # BANKs in = ',I6,/,
     +       ' # GETs     = ',I6,/,
     +       ' # GETKs    = ',I6,/,
     +       ' # SHOWs    = ',I6,/,
     +       ' # SCANs    = ',I6,/,
     +       ' # LOOPs    = ',I6,/,
     +       ' # LDIRs    = ',I6,/,
     +       ' # LFILs    = ',I6,/,
     +       ' # SORTs    = ',I6,/,
     +       ' # RANKs    = ',I6,/,
     +       ' # SELKs    = ',I6,/,
     +       ' # MATCHes  = ',I6,/)
 
      END
 
