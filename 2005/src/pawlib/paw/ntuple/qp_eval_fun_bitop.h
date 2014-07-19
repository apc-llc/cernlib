/*
 *  qp_eval_fun_bitop.h  --
 *	Implement the BITPAK bit handling routines
 *
 *  Original: 27-Jan-1995 15:07
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_eval_fun_bitop.h,v 1.4 1996/04/23 18:38:18 maartenb Exp $
 *
 *  $Log: qp_eval_fun_bitop.h,v $
 *  Revision 1.4  1996/04/23 18:38:18  maartenb
 *  - Add RCS keywords
 *
 *
 *  OP_FIELD	the name of the field in the Value structure union
 *  OP_TYPE	the type for declarations and casts
 *  OP_DTYPE	The Datatype value for this type
 *
 */


case FC_IOR + 16 * (OP_DTYPE - 1):
	r->u.OP_FIELD = valv[0]->u.OP_FIELD | valv[1]->u.OP_FIELD;
	r->d = dim_new( OP_DTYPE );
	break;

case FC_IAND + 16 * (OP_DTYPE - 1):
	r->u.OP_FIELD = valv[0]->u.OP_FIELD & valv[1]->u.OP_FIELD;
	r->d = dim_new( OP_DTYPE );
	break;

case FC_INOT + 16 * (OP_DTYPE - 1):
	r->u.OP_FIELD =  ~valv[0]->u.OP_FIELD;
	r->d = dim_new( OP_DTYPE );
	break;

case FC_IEOR + 16 * (OP_DTYPE - 1):
	r->u.OP_FIELD = valv[0]->u.OP_FIELD ^ valv[1]->u.OP_FIELD;
	r->d = dim_new( OP_DTYPE );
	break;

case FC_ISHFT + 16 * (OP_DTYPE - 1):
	r->u.OP_FIELD = valv[0]->u.OP_FIELD << valv[1]->u.ival;
	r->d = dim_new( OP_DTYPE );
	break;

case FC_ISHFTC + 16 * (OP_DTYPE - 1):
	{
		OP_TYPE	m, blob, mask;
		int	k, ic;

		m = valv[0]->u.OP_FIELD;
		k = valv[1]->u.ival;
		ic = valv[2]->u.ival;

		if ( (ic <= 0) || (ic > 8 * sizeof( OP_TYPE )) ) {
			sf_report( "ISHFT: arg 3 (%d) outside range [1..%d]\n",
				ic, 8 * sizeof( OP_TYPE ) );
			*err = R_MATH_ERROR;
			break;
		}

		k = k % ic;
		k = (k + ic) % ic; /* k is now positive and less than ic */
		if ( ic == (8 * sizeof( OP_TYPE )) ) {
			mask = ~ 0;
		} else {
			mask = (1 << ic) - 1;
		}
		blob = m & mask;

		blob = ((blob >> (ic - k)) | (blob << k)) & mask;
		m = ( m & ~ mask ) | blob;

		r->u.OP_FIELD = m;
		r->d = dim_new( OP_DTYPE );
		break;
	}

case FC_IBITS + 16 * (OP_DTYPE - 1):
	{
		OP_TYPE	mask;
		int	i, len;

		i = valv[1]->u.ival;
		len = valv[2]->u.ival;

		if ( (i < 0) || (i >= 8 * sizeof( OP_TYPE )) ) {
			sf_report( "IBITS: arg 2 (%d) outside range [0..%d]\n",
				i, 8 * sizeof( OP_TYPE ) - 1 );
			*err = R_MATH_ERROR;
			break;
		}
		if ( (len <= 0) || (i+len > 8 * sizeof( OP_TYPE )) ) {
			sf_report( "IBITS: arg 3 (%d) outside range [1..%d] "
				"(arg 2 is %d)\n", len, 8 * sizeof( OP_TYPE )-i, i );
			*err = R_MATH_ERROR;
			break;
		}

		if ( len == (8 * sizeof( OP_TYPE )) ) {
			mask = ~ 0;
		} else {
			mask = (1 << len) - 1;
		}
		r->u.OP_FIELD = (valv[0]->u.OP_FIELD >> i) & mask;
		r->d = dim_new( OP_DTYPE );

		break;
	}


case FC_MVBITS + 16 * (OP_DTYPE - 1):
	{
		OP_TYPE	mask;
		int	i, len, j;

		i = valv[1]->u.ival;
		len = valv[2]->u.ival;
		j = valv[4]->u.ival;

		if ( (i < 0) || (i >= 8 * sizeof( OP_TYPE )) ) {
			sf_report( "MVBITS: arg 2 (%d) outside range [0..%d]\n",
				i, 8 * sizeof( OP_TYPE ) - 1 );
			*err = R_MATH_ERROR;
			break;
		}
		if ( (j < 0) || (j >= 8 * sizeof( OP_TYPE )) ) {
			sf_report( "MVBITS: arg 5 (%d) outside range [0..%d]\n",
				j, 8 * sizeof( OP_TYPE ) - 1 );
			*err = R_MATH_ERROR;
			break;
		}
		if ( (len <= 0) || (i+len > 8 * sizeof( OP_TYPE )) ) {
			sf_report( "MVBITS: arg 3 (%d) outside range [1..%d] "
				"(arg 2 is %d)\n", len, 8 * sizeof( OP_TYPE )-i, i );
			*err = R_MATH_ERROR;
			break;
		}
		if ( (j+len > 8 * sizeof( OP_TYPE )) ) {
			sf_report( "MVBITS: arg 3 (%d) outside range [1..%d] "
				"(arg 4 is %d)\n", len, 8 * sizeof( OP_TYPE )-j, j );
			*err = R_MATH_ERROR;
			break;
		}

		if ( len == (8 * sizeof( OP_TYPE )) ) {
			mask = ~ 0;
		} else {
			mask = (1 << len) - 1;
		}

		r->u.OP_FIELD = (((valv[0]->u.OP_FIELD >> i) & mask) << j)
				| (valv[3]->u.OP_FIELD & ~ (mask << j)) ;
		r->d = dim_new( OP_DTYPE );

		break;
	}

case FC_BTEST + 16 * (OP_DTYPE - 1):
	r->u.bval = (valv[0]->u.OP_FIELD & ( 1 << valv[1]->u.ival)) != 0 ;
	r->d = dim_new( D_BOOL );
	break;

case FC_IBSET + 16 * (OP_DTYPE - 1):
	r->u.OP_FIELD = valv[0]->u.OP_FIELD | ( 1 << valv[1]->u.ival);
	r->d = dim_new( OP_DTYPE );
	break;

case FC_IBCLR + 16 * (OP_DTYPE - 1):
	r->u.OP_FIELD = valv[0]->u.OP_FIELD & ~ ( 1 << valv[1]->u.ival);
	r->d = dim_new( OP_DTYPE );
	break;

case FC_IMOD + 16 * (OP_DTYPE - 1):
	if ( valv[1]->u.OP_FIELD != 0 ) {
		r->u.OP_FIELD = valv[0]->u.OP_FIELD %
					valv[1]->u.OP_FIELD;
	} else {
		sf_report( "eval_template: modulo by zero\n" );
		*err = R_MATH_ERROR;
		r->u.OP_FIELD = 0;
	}
	r->d = dim_new( OP_DTYPE );
	break;

case FC_ISIGN + 16 * (OP_DTYPE - 1):
	r->u.OP_FIELD = valv[0]->u.OP_FIELD | valv[1]->u.OP_FIELD;
	r->d = dim_new( OP_DTYPE );
	break;

case FC_IABS + 16 * (OP_DTYPE - 1):
	r->u.OP_FIELD =  valv[0]->u.OP_FIELD > 0 ?
			valv[0]->u.OP_FIELD : -valv[0]->u.OP_FIELD;
	r->d = dim_new( OP_DTYPE );
	break;

