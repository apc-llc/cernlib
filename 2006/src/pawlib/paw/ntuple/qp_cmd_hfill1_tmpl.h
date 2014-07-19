/*
 *  qp_cmd_hfill1_tmpl.h  --
 *
 *  Original: 24-Apr-1995 18:07
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_cmd_hfill1_tmpl.h,v 1.5 1996/04/23 18:38:09 maartenb Exp $
 *
 *  $Log: qp_cmd_hfill1_tmpl.h,v $
 *  Revision 1.5  1996/04/23 18:38:09  maartenb
 *  - Add RCS keywords
 *
 *
 */

{
	register OP_TYPE		*o1;
	register Float32		*o2;
	register int			n, inc_o1, inc_o2;

	o1 = (OP_TYPE *) &stack[frame[framep]];
	n = frame_size[framep];
	if ( n != 1 ) {
		inc_o1 = 1;
	} else {
		inc_o1 = 0;
	}

	o2 = (Float32 *) &stack[frame[framep-1]];
	if ( frame_size[framep-1] != 1 ) {	/* allow 0 and > 1 */
		if ( n == 1 ) {
			n = frame_size[framep-1];
		} else {
			qp_assert( n == frame_size[framep-1] );
		}
		inc_o2 = 1;
	} else {
		inc_o2 = 0;
	}

	for ( ; n > 0 ; n-- ) {
		if ( *o2 != 0. ) {
			hfill_x[hfill_n] = *o1;
			hfill_w[hfill_n] = *o2;
			hfill_n += 1;

			if ( hfill_n == HFILL_BSIZE ) {
				HF1N( qp_hfill_id, hfill_x, hfill_w,
					HFILL_BSIZE );
				hfill_n = 0;
			}
		}
		o1 += inc_o1;
		o2 += inc_o2;
	}
}
