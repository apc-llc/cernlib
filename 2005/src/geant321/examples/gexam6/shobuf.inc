*
* $Id: shobuf.inc,v 1.1.1.1 1995/10/24 10:22:19 cernlib Exp $
*
* $Log: shobuf.inc,v $
* Revision 1.1.1.1  1995/10/24 10:22:19  cernlib
* Geant
*
*
*
* shobuf.inc
*
      PARAMETER (NCELL=40,NWORD=2*NCELL,NBYTE=4*NWORD)
C
      COMMON/SHOBUF/NXBIN,NYBIN,NZBIN,XBIN,YBIN,ZBIN,
     +              NENERG,ESHOW(20),NSHOW(20),NFIRE(20),NRCOR(20),
     +              XGENER,YGENER,ZGENER,CUTLOW,
     +              IENERG,ISHOW,IRECOR,
     +              IENBUF(NWORD),IBUF(NWORD)
C
      DIMENSION     ENBUF(NWORD),BUF(NWORD)
      EQUIVALENCE (ENBUF,IENBUF) , (BUF,IBUF)
C
C     NCELL = number of cells per logical record
C     NWORD = number of words per logical record
C     NBYTE = logical record length in bytes
C     IENBUF= directory record for a given energy
C     IBUF  = record of shower informations
C
C       One must store at least 2 informations per cell fired :
C          - the cell number
C          - the energy deposited in the cell
C
