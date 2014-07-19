*
* $Id: csfmt.inc,v 1.1.1.1 1996/02/26 17:16:35 mclareni Exp $
*
* $Log: csfmt.inc,v $
* Revision 1.1.1.1  1996/02/26 17:16:35  mclareni
* Comis
*
*
#ifndef CERNLIB_COMIS_CSFMT_INC
#define CERNLIB_COMIS_CSFMT_INC
*
*
* csfmt.inc
*
      CHARACTER BUF*132,FMT*512
      COMMON/CSFMTS/BUF,FMT
      COMMON/CSFMTI/IFMTST,JIOSTA,LIOERR,LIOEND,LUNIO,KEYRW,JSTR,LENSTR,
     +              IBF,IBFLST,LBUF,IFMT,NREP,ICODE,IWIDTH,IPOS1,IPOS2,
     +              NDAREC,IUBS,LUBTOT,LULIST,IUBTOP,IUBUF(256),
     +              JADRLN(14,2)

#endif
