*
* $Id: eppaccom.inc,v 1.1.1.1 1996/03/08 15:21:54 mclareni Exp $
*
* $Log: eppaccom.inc,v $
* Revision 1.1.1.1  1996/03/08 15:21:54  mclareni
* Epio
*
*
* COPY OF EPIOCOM KEEP SEQUENCE FOR BACK COMPATIBILTY.
*
* eppaccom.inc
*
#if defined(CERNLIB_ND50)||defined(CERNLIB_ND500)
      COMMON/EPCOMM/NMUNIT,NWUNIT,NCONT,ISTART,LASTUT,LREF,LIST(350)
      EQUIVALENCE (NOUTUT,LIST(8))
#endif
