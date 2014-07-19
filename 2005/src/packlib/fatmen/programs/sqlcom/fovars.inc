*
* $Id: fovars.inc,v 1.1.1.1 1996/03/07 15:17:49 mclareni Exp $
*
* $Log: fovars.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:49  mclareni
* Fatmen
*
*
*    Common variables definition (separated because of PCC)
*
* fovars.inc
*
 
*...  Host variables for GNAME_ table
 
      CHARACTER*240 GENAME(50)
 
*...  Host variables for  FILES_ table
 
      INTEGER CPLVL(50),  LOCAT(50),  SRTREC(50), ENDREC(50),
     1        SRTBLK(50), ENDBLK(50), RECLGH(50), BLKLGH(50),
     2        PROTEC(50), USRWD0(50), USRWD1(50), USRWD2(50),
     3        USRWD3(50), USRWD4(50), USRWD5(50), USRWD6(50),
     4        USRWD7(50), USRWD8(50), USRWD9(50), SYSWD0(50),
     5        SYSWD1(50), SYSWD2(50), SYSWD3(50), SYSWD4(50),
     6        SYSWD5(50), SYSWD6(50), SYSWD7(50), SYSWD8(50),
     7        SYSWD9(50), FLSQNO(50), VLSQNO(50)
 
      CHARACTER HNAME(50)*8,  FNAME(50)*240,HTYPE(50)*16, OPSYS(50)*12,
     1          FFORMT(50)*4, USRFMT(50)*4, RECFMT(50)*4, CREDAT(50)*15,
     2          CATDAT(50)*15,ACSDAT(50)*15,CRENAM(50)*8, CREACC(50)*8,
     3          CRENOD(50)*8, CREJOB(50)*8, COMMTS(50)*80,MEDIA(50)*1,
     4          ACTIVE(50)*1
 
*...  Host variables for GNAME_ table
 
      INTEGER PREF(50), DENS(50)
      CHARACTER VID(50)*6, VSN(50)*6
 
