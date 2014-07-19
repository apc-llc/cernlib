/*
 * $Id: kipiaf.c,v 1.1.1.1 1996/03/08 15:32:57 mclareni Exp $
 *
 * $Log: kipiaf.c,v $
 * Revision 1.1.1.1  1996/03/08 15:32:57  mclareni
 * Kuip
 *
 */
/*CMZ :  2.03/07 27/08/93  15.31.11  by  Alfred Nathaniel*/
/*-- Author :    Alfred Nathaniel   15/04/93*/
/* kipiaf.c: interface to Piaf server */

#include "kuip/kuip.h"
#include "kuip/kfor.h"


static struct {
  SUBROUTINE *czgeta_F;
  SUBROUTINE *czputa_F;
  SUBROUTINE *fzin_F;
  SUBROUTINE *fzout_F;
} kjmppiaf;


/*
 * This routine has to be called by the Piaf server to define the routines
 * needed to transfer vectors.
 */
void F77_ENTRY_E4(Kipiaf,czgeta,czputa,fzin,fzout)
/* { */
  kjmppiaf.czgeta_F = czgeta;
  kjmppiaf.czputa_F = czputa;
  kjmppiaf.fzin_F = fzin;
  kjmppiaf.fzout_F = fzout;
}


/*
 * This routine is called by KUVECT if a vector is not defined.
 * If we are on the Piaf server we try to import the vector from the client
 * and return LVECN.NE.0
 */
void F77_ENTRY_CI(Kivect,chvec,lvecn)
/* { */

  *lvecn = 0;

#ifdef UNIX                             /* Piaf server only be on Unix */
  if( kjmppiaf.czgeta_F != NULL ) {
    /* we are on the server side */
    char chmail[80];
    char *msg;
    INTEGER istat;

    /* ask client about this vector */
    msg = mstrncat( strdup( "KISNDV" ), chvec, len_chvec );
    F77_CALL_Cx(kjmppiaf.czputa_F,msg,strlen(msg),&istat);
    free( msg );

    F77_CALL_Cx(kjmppiaf.czgeta_F,chmail,(sizeof chmail),&istat);

    if( chmail[0] != ' ' ) {
      /* client knows this vector */
      INTEGER idim[3];
      int ntot = 1;
      int nbuf = 400;
      int dim[3];
      int i;

      /* client returns type and dimensions */
      sscanf( chmail + 1, " %d %d %d %d", &nbuf, &dim[0], &dim[1], &dim[2] );
      for( i = 0; i < 3; i++ ) {
        idim[i] = dim[i];
        ntot *= dim[i];
      }

      /* create vector: KXCRV2 also updates LVECN */
      F77_CALL_CxC(Kxcrv2,chvec,len_chvec,idim,chmail,1);

      for( i = 0; i < ntot; i += nbuf ) {
        INTEGER lun = 999;
        INTEGER ixdiv = Pawc->IXKUIP;
        INTEGER lsup = 0;
        INTEGER jbias = 1;
        INTEGER nuh = ntot - i;
        if( nuh > nbuf )
          nuh = nbuf;
        F77_CALL_x4Cxx((*kjmppiaf.fzin_F),
                       &lun,&ixdiv,&lsup,&jbias," ",1,&nuh,&IQ(*lvecn+15+i));
      }
    }
  }
#endif
}
