/*
 *  qp_exe_fun_const.h  --
 *
 *  Original: 24-Apr-1995 17:02
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_fun_const.h,v 1.4 1996/04/23 18:38:29 maartenb Exp $
 *
 *  $Log: qp_exe_fun_const.h,v $
 *  Revision 1.4  1996/04/23 18:38:29  maartenb
 *  - Add RCS keywords
 *
 *
 */

case FC_TRUE:
{
	NEW_FRAME( D_BOOL, 1, r );
	*(bool *) r = TRUE;
	break;
}

case FC_FALSE:
{
	NEW_FRAME( D_BOOL, 1, r );
	*(bool *) r = FALSE;
	break;
}

case FC_PI:
{
	NEW_FRAME( D_FLOAT, 1, r );
	*(Float32 *) r = M_PI;
	break;
}

case FC_DPI:
{
	NEW_FRAME( D_DOUBLE, 1, r );
	*(Float64 *) r = M_PI;
	break;
}


case FC_UINT32_MIN:
{
	NEW_FRAME( D_UINT, 1, r );
	*(UInt32 *) r = 0;
	break;
}

case FC_UINT32_MAX:
{
	NEW_FRAME( D_UINT, 1, r );
	*(UInt32 *) r = UINT32_MAX;
	break;
}

case FC_UINT64_MIN:
{
	NEW_FRAME( D_ULONG, 1, r );
	*(UInt64 *) r = 0;
	break;
}

case FC_UINT64_MAX:
{
	NEW_FRAME( D_ULONG, 1, r );
	*(UInt64 *) r = UINT64_MAX;
	break;
}


case FC_INT32_MIN:
{
	NEW_FRAME( D_INT, 1, r );
	*(Int32 *) r = INT32_MIN;
	break;
}

case FC_INT32_MAX:
{
	NEW_FRAME( D_INT, 1, r );
	*(Int32 *) r = INT32_MAX;
	break;
}

case FC_INT64_MIN:
{
	NEW_FRAME( D_LONG, 1, r );
	*(Int64 *) r = INT64_MIN;
	break;
}

case FC_INT64_MAX:
{
	NEW_FRAME( D_LONG, 1, r );
	*(Int64 *) r = INT64_MAX;
	break;
}


case FC_FLOAT32_MIN:
{
	NEW_FRAME( D_FLOAT, 1, r );
	*(Float32 *) r = FLOAT32_MIN;
	break;
}

case FC_FLOAT32_MAX:
{
	NEW_FRAME( D_FLOAT, 1, r );
	*(Float32 *) r = FLOAT32_MAX;
	break;
}

case FC_FLOAT64_MIN:
{
	NEW_FRAME( D_DOUBLE, 1, r );
	*(Float64 *) r = FLOAT64_MIN;
	break;
}

case FC_FLOAT64_MAX:
{
	NEW_FRAME( D_DOUBLE, 1, r );
	*(Float64 *) r = FLOAT64_MAX;
	break;
}


