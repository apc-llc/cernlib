*
* $Id: zbrcdf.cdf,v 1.1.1.1 1996/03/04 16:12:51 mclareni Exp $
*
* $Log: zbrcdf.cdf,v $
* Revision 1.1.1.1  1996/03/04 16:12:51  mclareni
* Dzdoc/Zebpack
*
*
 
>NAME ZBRDEF
 
>Graphics
>Browse Zebra ' ' DZDZEB
List
'Open bank doc Rzfile' . 'DZDOC/FILE/OPEN_DOC'
'Add doc directory'    . 'DZDOC/FILE/DIR_ADD'
'Put doc into Rzfile'  . 'DZDOC/FILE/EDIT_DOC'
'Display bank tree'    . 'DZDOC/BANKDISPLAY/TREE'
 
>Class /STORE Stores big_store sm_dir
List
'Show store DZSTOR'    . 'SHOW_STORE [this]'
 
>Class /DIV  Divisions big_div sm_key
List
'Display division'    . 'BROWSE/SHOW_DIV [this]'
'Snap division'       . 'BROWSE/SNAP_DIV [this]'
'Verify division'     . 'BROWSE/VERIFY_DIV [this]'
'Collect garbage'     . 'BROWSE/COL_GARB [this]'
'Set filter for banks' . 'BROWSE/FILT_BANKS'
 
>Class BANK Banks big_bank sm_key
'Display bank tree'   . ' ' DZDB01
'Show cont documentd' . ' ' DZDB03
'DZ Show contents'    . ' ' DZDB04
'Show system words'   . ' ' DZDB05
'Survey bank tree'    . ' ' DZDB06
'Put into vector'     . 'BROWSE/PT_VECT  [that]'
'Show documention'    . ' ' DZDB07
'Edit documention'    . ' ' DZDB08
'Modify data words'   . 'BROWSE/SH_MODIFY [that]'
'Drop bank (tree)'    . 'BROWSE/SH_DROP [that]'
'Write fo FZ file'    . 'BROWSE/SH_FOUT [that]'
+
'Display bank tree'   . ' '  DZDB11
'More down banks'     . ' '  DZDB12
'Show cont documentd' . ' '  DZDB13
'DZ Show contents'    . ' '  DZDB14
'Show sytem words'    . ' '  DZDB15
'Survey bank tree'    . ' '  DZDB16
'Put into vector'     . 'BROWSE/PT_VECT  -[this]'
'Show documention'    . ' '  DZDB17
'Edit documention'    . ' '  DZDB18
'Mark for Ntuple'     . 'BROWSE/MARK_NT  -[this]'
'Modify data words'   . 'BROWSE/SH_MODIFY -[this]'
'Drop bank (tree)'    . 'BROWSE/SH_DROP -[this]'
'Write fo FZ file'    . 'BROWSE/SH_FOUT -[this]'
 
>Class /rzfil Rzfile rzfile sm_dir
List
'Close Rzfile'         . 'BROWSE/CLOSE_RZ [this]'
'Show status'          . 'BROWSE/SHOW_RZS [this]'
 
>Class /rzdir Rz-directory rzdir sm_dir
List
'List directory (RZLDIR)' . 'BROWSE/SH_RZLDIR  [path]/[this]'
'Show key definition'    . 'BROWSE/SH_KEY_DEF  [path]/[this]'
'Set filter on keys'     . 'BROWSE/FILT_KEY'
 
>Class key Keys big_key sm_key
'Read key into memory' . 'BROWSE/GET_KEY [path] [that]'
'Show key definition'  . 'BROWSE/SH_KEY_DEF  [path]/[this]'
'Show key words'       . 'BROWSE/SH_KEY  [path] [that]'
'Set filter on keys'    . 'BROWSE/FILT_KEY'
 
 
>Class FZ Fzfiles fzfile sm_dir
'Get event (head + d/s)' . 'BROWSE/GET_EVENT [that] D'
'Get user header'        . 'BROWSE/GET_HEAD  [that] D'
'Get the pending d/s '   . 'BROWSE/GET_PDS   [that] D'
'List marked for Ntuple' . 'BROWSE/LIST_NTUPLE '
'Fill Ntuple'            . 'BROWSE/FILL_NTUPLE [that]'
'Show Fzfile info'       . 'BROWSE/SH_FZINFO [that]'
'Set logging level'      . 'BROWSE/FZ_LOG_LEVEL [that]'
'Rewind Fzfile'          . 'BROWSE/REWIND_FZ  [that]'
'Close Fzfile'           . 'BROWSE/CLOSE_FZ  [that]'
 
>Icon_bitmaps
#define rzfile_width 30
#define rzfile_height 30
static char rzfile_bits[] = {
   0x01, 0xfe, 0xff, 0xff, 0xfc, 0x00, 0x00, 0xe0, 0xfe, 0x01, 0x08, 0xe0,
   0x86, 0x01, 0x14, 0xe0, 0x86, 0x01, 0x3e, 0xe0, 0xfe, 0x00, 0x55, 0xe0,
   0x7e, 0x80, 0xff, 0xe0, 0x66, 0xc0, 0xd5, 0xe1, 0xe6, 0x60, 0x7f, 0xe3,
   0xc6, 0xa1, 0xef, 0xe3, 0x86, 0x21, 0xfb, 0xe2, 0x00, 0x50, 0x5e, 0xe3,
   0x01, 0xa8, 0xb4, 0xe2, 0x01, 0x44, 0x59, 0xe3, 0x01, 0x8c, 0xaa, 0xe2,
   0x00, 0x14, 0x5d, 0xf3, 0xfe, 0x25, 0xae, 0xf2, 0xfe, 0x45, 0x5b, 0xf3,
   0xc0, 0x85, 0xa9, 0xf2, 0xe0, 0x84, 0x5c, 0xf3, 0x70, 0x88, 0xae, 0xf2,
   0x38, 0x90, 0x5b, 0xf1, 0x1c, 0xa0, 0xa9, 0xf0, 0x0e, 0xc0, 0x59, 0xf0,
   0xfe, 0x80, 0x2a, 0xf0, 0xfe, 0x01, 0x1c, 0xf0, 0x00, 0x00, 0x08, 0xf0,
   0x01, 0x00, 0x00, 0xf0, 0x01, 0xe0, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};
 
#define fzfile_width 30
#define fzfile_height 30
static char fzfile_bits[] = {
   0x00, 0x00, 0x00, 0xe0, 0xfe, 0x03, 0x00, 0xe0, 0xfe, 0x03, 0x08, 0xe0,
   0x06, 0x00, 0x14, 0xe0, 0x06, 0x00, 0x3e, 0xe0, 0x7e, 0x00, 0x55, 0xe0,
   0x7e, 0x80, 0xff, 0xe0, 0x06, 0xc0, 0xd5, 0xe1, 0x06, 0x60, 0x7f, 0xe3,
   0x06, 0xa0, 0xef, 0xe3, 0x06, 0x20, 0xfb, 0xe2, 0x06, 0x50, 0x5e, 0xe3,
   0x06, 0xa8, 0xb4, 0xe2, 0x00, 0x44, 0x59, 0xe3, 0x00, 0x8c, 0xaa, 0xe2,
   0x00, 0x14, 0x5d, 0xe3, 0xfe, 0x25, 0xae, 0xe2, 0xfe, 0x45, 0x5b, 0xe3,
   0xc0, 0x95, 0xa9, 0xe2, 0xe0, 0xa4, 0x5c, 0xe3, 0x70, 0x88, 0xae, 0xe2,
   0x38, 0x90, 0x5b, 0xe1, 0x1c, 0xa0, 0xa9, 0xe0, 0x0e, 0xc0, 0x59, 0xe0,
   0xfe, 0x81, 0x2a, 0xe0, 0xfe, 0x01, 0x1c, 0xe0, 0x00, 0x00, 0x08, 0xe0,
   0x00, 0x00, 0x00, 0xe0, 0x00, 0x00, 0x00, 0xe0, 0xff, 0xff, 0xff, 0xff};
 
#define rzdir_width 30
#define rzdir_height 23
static char rzdir_bits[] = {
   0xff, 0xff, 0xff, 0x3f, 0x01, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x30,
   0xa9, 0xaa, 0xaa, 0x3a, 0x51, 0x55, 0x55, 0x35, 0xa9, 0xaa, 0xaa, 0x3a,
   0x01, 0x54, 0x15, 0x30, 0x01, 0xa8, 0x0a, 0x30, 0x51, 0x01, 0x40, 0x35,
   0xa9, 0x02, 0xa0, 0x3a, 0x51, 0x55, 0x55, 0x35, 0xa9, 0xaa, 0xaa, 0x3a,
   0xd1, 0x5f, 0xff, 0x34, 0xe9, 0x1f, 0xff, 0x3a, 0xd1, 0x5c, 0x75, 0x35,
   0xe9, 0xae, 0xba, 0x3a, 0xd1, 0x57, 0x5d, 0x35, 0xe9, 0xae, 0xae, 0x3a,
   0xd1, 0x5c, 0xff, 0x34, 0xe9, 0xba, 0xff, 0x3a, 0x51, 0x55, 0x55, 0x35,
   0xfd, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f};
 
#define big_key_width 30
#define big_key_height 30
static char big_key_bits[] = {
   0x00, 0x00, 0x00, 0xc0, 0xe0, 0x1f, 0x00, 0xc0, 0x78, 0xf0, 0xff, 0xcf,
   0x18, 0xc0, 0xff, 0xcf, 0x0c, 0x80, 0xff, 0xcf, 0x8c, 0x87, 0xff, 0xcf,
   0xc4, 0x0f, 0xff, 0xcf, 0xc4, 0x1f, 0xff, 0xcf, 0xc4, 0x1f, 0xff, 0xcf,
   0xc4, 0x1f, 0xff, 0xcf, 0xc4, 0x0f, 0xff, 0xcf, 0x8c, 0x87, 0xff, 0xcf,
   0x0c, 0x00, 0xff, 0xcf, 0x1c, 0x00, 0xfe, 0xcf, 0x7c, 0x10, 0xfc, 0xcf,
   0xfc, 0x3f, 0xf8, 0xcf, 0xfc, 0x7f, 0xf0, 0xcf, 0xfc, 0xff, 0xe0, 0xcf,
   0xfc, 0xff, 0xc1, 0xcf, 0xfc, 0xff, 0x83, 0xcf, 0xfc, 0xff, 0x03, 0xcf,
   0xfc, 0xff, 0x01, 0xce, 0xfc, 0xff, 0x00, 0xce, 0xfc, 0x3f, 0x00, 0xcf,
   0xfc, 0x1f, 0x80, 0xcf, 0xfc, 0x1f, 0xc1, 0xcf, 0xfc, 0x9f, 0xe7, 0xcf,
   0xfc, 0x9f, 0xf1, 0xcf, 0x00, 0xf8, 0x7f, 0xc0, 0x00, 0x00, 0x00, 0xc0};
 
#define big_div_width 30
#define big_div_height 30
static char big_div_bits[] = {
   0xff, 0xff, 0xff, 0x3f, 0x01, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x20,
   0xf1, 0x1f, 0xfc, 0x27, 0xf9, 0x1f, 0xfe, 0x27, 0xfd, 0x17, 0xff, 0x25,
   0x25, 0x14, 0x09, 0x25, 0x05, 0x04, 0x01, 0x21, 0x05, 0x04, 0x01, 0x21,
   0x01, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x20,
   0xf1, 0x1f, 0xfc, 0x27, 0xf9, 0x1f, 0xfe, 0x27, 0xfd, 0x17, 0xff, 0x25,
   0x25, 0x14, 0x09, 0x25, 0x05, 0x04, 0x01, 0x21, 0x05, 0x04, 0x01, 0x21,
   0x01, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x20,
   0x01, 0x00, 0x00, 0x20, 0xf1, 0x1f, 0xfc, 0x27, 0xf9, 0x1f, 0xfe, 0x27,
   0xfd, 0x17, 0xff, 0x25, 0x25, 0x14, 0x09, 0x25, 0x05, 0x04, 0x01, 0x21,
   0x05, 0x04, 0x01, 0x21, 0x01, 0x00, 0x00, 0x20, 0xff, 0xff, 0xff, 0x3f};
 
#define big_bank_width 30
#define big_bank_height 30
static char big_bank_bits[] = {
   0xff, 0xff, 0xff, 0x3f, 0x81, 0x00, 0x00, 0x20, 0x81, 0x01, 0x00, 0x20,
   0xc1, 0x03, 0x00, 0x20, 0x41, 0x06, 0x00, 0x20, 0xa1, 0x0c, 0x00, 0x20,
   0x31, 0x38, 0x00, 0x20, 0x19, 0xfc, 0xf0, 0x27, 0x89, 0xe6, 0x7f, 0x24,
   0xcd, 0x33, 0x63, 0x2c, 0x2d, 0x39, 0x73, 0x3e, 0x19, 0x9b, 0x31, 0x3b,
   0x01, 0x9e, 0x39, 0x29, 0x01, 0x9c, 0x99, 0x35, 0x01, 0xec, 0xf8, 0x25,
   0x01, 0xdc, 0x5d, 0x34, 0x01, 0x2c, 0xcf, 0x32, 0x01, 0x1a, 0x80, 0x32,
   0x01, 0x0e, 0x80, 0x22, 0x01, 0x0e, 0x80, 0x22, 0x01, 0x06, 0x80, 0x21,
   0x01, 0x07, 0xc0, 0x21, 0x01, 0x00, 0x00, 0x20, 0xe1, 0xff, 0xff, 0x2f,
   0x31, 0x00, 0x00, 0x2e, 0xf9, 0xff, 0xff, 0x2b, 0x59, 0x00, 0x00, 0x2b,
   0x59, 0x00, 0x00, 0x2b, 0x19, 0x00, 0x00, 0x23, 0xff, 0xff, 0xff, 0x3f};
 
#define big_store_width 30
#define big_store_height 30
static char big_store_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x18,
   0x00, 0xff, 0xff, 0x1f, 0x80, 0xff, 0xff, 0x1f, 0xc0, 0x06, 0x00, 0x1e,
   0x78, 0x06, 0x60, 0x1b, 0xf8, 0xff, 0xff, 0x19, 0xf8, 0xff, 0xff, 0x18,
   0x18, 0x06, 0x60, 0x18, 0x18, 0x06, 0x60, 0x18, 0x18, 0x06, 0x60, 0x18,
   0x18, 0xff, 0xff, 0x1f, 0x98, 0x06, 0x60, 0x1c, 0x58, 0x06, 0x60, 0x1a,
   0x38, 0x00, 0x60, 0x19, 0xf8, 0xff, 0xff, 0x18, 0x18, 0x06, 0x60, 0x18,
   0x18, 0xfe, 0xff, 0x1f, 0x18, 0xff, 0xff, 0x1f, 0x98, 0x07, 0x60, 0x1c,
   0xd8, 0x06, 0x60, 0x1e, 0x78, 0x00, 0x60, 0x1b, 0xf8, 0xff, 0xff, 0x01,
   0xf8, 0xff, 0xff, 0x00, 0x18, 0x00, 0x60, 0x00, 0x18, 0x00, 0x60, 0x00,
   0x18, 0x00, 0x60, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
 
#define big_fzfile_width 30
#define big_fzfile_height 30
static char big_fzfile_bits[] = {
   0xff, 0xff, 0xff, 0x3f, 0x01, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x20,
   0xf9, 0x0f, 0x00, 0x20, 0x01, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x20,
   0xf9, 0xff, 0x03, 0x20, 0x01, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x20,
   0xf9, 0xff, 0x7f, 0x20, 0x01, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x20,
   0x01, 0x00, 0x00, 0x20, 0xc1, 0x7f, 0xfe, 0x23, 0xc1, 0x7f, 0xfe, 0x23,
   0xc1, 0x00, 0x80, 0x23, 0xc1, 0x00, 0xc0, 0x21, 0xc1, 0x00, 0xe0, 0x20,
   0xc1, 0x0f, 0x70, 0x20, 0xc1, 0x0f, 0x70, 0x20, 0xc1, 0x00, 0x38, 0x20,
   0xc1, 0x00, 0x1c, 0x20, 0xc1, 0x00, 0x0e, 0x20, 0xc1, 0x00, 0xfe, 0x23,
   0xc1, 0x00, 0xfe, 0x23, 0x01, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x20,
   0x01, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x20, 0xff, 0xff, 0xff, 0x3f};
#define big_cart_width 30
#define big_cart_height 30
static char big_cart_bits[] = {
   0xff, 0xff, 0xff, 0x3f, 0x01, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x20,
   0x01, 0x00, 0x00, 0x20, 0x01, 0xfe, 0xff, 0x21, 0x01, 0xff, 0xff, 0x23,
   0x81, 0xff, 0xff, 0x23, 0xc1, 0xff, 0xff, 0x23, 0xe1, 0xff, 0xff, 0x23,
   0xf1, 0xff, 0xff, 0x23, 0xf1, 0x1f, 0xfc, 0x23, 0xf1, 0x8f, 0xf8, 0x23,
   0xf1, 0x07, 0xf0, 0x23, 0xf1, 0xc7, 0xf1, 0x23, 0xf1, 0x47, 0xf1, 0x23,
   0xf1, 0xc7, 0xf1, 0x23, 0xf1, 0x17, 0xf4, 0x23, 0xf1, 0x0f, 0xf8, 0x23,
   0xf1, 0x1f, 0xfc, 0x23, 0xf1, 0xff, 0xff, 0x23, 0xf1, 0xff, 0xff, 0x23,
   0xf1, 0xff, 0xff, 0x23, 0xf1, 0xff, 0xff, 0x23, 0xf1, 0xff, 0xff, 0x23,
   0xf1, 0xff, 0xff, 0x23, 0xe1, 0xff, 0xff, 0x21, 0x01, 0x00, 0x00, 0x20,
   0x01, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x20, 0xff, 0xff, 0xff, 0x3f};
#define big_tape_width 30
#define big_tape_height 30
static char big_tape_bits[] = {
   0x00, 0xf0, 0x07, 0x00, 0x00, 0x0e, 0x38, 0x00, 0x80, 0x01, 0xc0, 0x00,
   0x40, 0xe0, 0x03, 0x01, 0x20, 0xfc, 0x1f, 0x02, 0x10, 0xfe, 0x3f, 0x04,
   0x08, 0xff, 0x7f, 0x08, 0x88, 0xff, 0xff, 0x08, 0xc4, 0xff, 0xff, 0x11,
   0xe4, 0xff, 0xff, 0x13, 0xe4, 0xff, 0xff, 0x13, 0xe2, 0x1f, 0xfc, 0x27,
   0xf2, 0x0f, 0xf8, 0x27, 0xf2, 0x0f, 0xf8, 0x27, 0xf2, 0x0f, 0xf8, 0x27,
   0xf2, 0x0f, 0xf8, 0x27, 0xf2, 0x0f, 0xf8, 0x27, 0xe2, 0x1f, 0xfc, 0x27,
   0xe4, 0xff, 0xff, 0x13, 0xe4, 0xff, 0xff, 0x13, 0xc4, 0xff, 0xff, 0x11,
   0x88, 0xff, 0xff, 0x08, 0x08, 0xff, 0x7f, 0x08, 0x10, 0xfe, 0x3f, 0x04,
   0x20, 0xfc, 0x1f, 0x02, 0x40, 0xe0, 0x03, 0x01, 0x80, 0x01, 0xc0, 0x00,
   0x00, 0x0e, 0x38, 0x00, 0x00, 0xf0, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00};
 
#define big_key1_width 30
#define big_key1_height 30
static char big_key1_bits[] = {
   0xff, 0xff, 0xff, 0x3f, 0x01, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x30,
   0xa9, 0xaa, 0xaa, 0x3a, 0x51, 0x55, 0x55, 0x35, 0xf9, 0xff, 0xff, 0x3f,
   0x11, 0x00, 0x00, 0x34, 0x19, 0x00, 0x00, 0x3c, 0xf1, 0xff, 0xff, 0x37,
   0x59, 0x55, 0xb5, 0x3e, 0x31, 0x33, 0x13, 0x34, 0x19, 0x11, 0x31, 0x3c,
   0xf1, 0xff, 0xff, 0x37, 0x59, 0x55, 0xb5, 0x3e, 0x31, 0x33, 0x13, 0x34,
   0x19, 0x11, 0x31, 0x3c, 0xf1, 0xff, 0xff, 0x37, 0x59, 0x55, 0xb5, 0x3e,
   0x31, 0x33, 0x13, 0x34, 0x19, 0x11, 0x31, 0x3c, 0xf1, 0xff, 0xff, 0x37,
   0x59, 0x55, 0xb5, 0x3e, 0x31, 0x33, 0x13, 0x34, 0x19, 0x11, 0x31, 0x3c,
   0x31, 0x33, 0x13, 0x34, 0xf9, 0xff, 0xff, 0x3f, 0x51, 0x55, 0x55, 0x35,
   0xa9, 0xaa, 0xaa, 0x3a, 0xfd, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f};
 
#define sm_key_width 20
#define sm_key_height 20
static char sm_key_bits[] = {
   0xff, 0xff, 0x0f, 0x01, 0x00, 0x08, 0x01, 0x00, 0x0c, 0xa9, 0xaa, 0x0e,
   0xf1, 0xff, 0x0d, 0x19, 0x00, 0x0f, 0xf1, 0xff, 0x0d, 0xd9, 0xb6, 0x0f,
   0xb1, 0x6d, 0x0d, 0x99, 0x24, 0x0f, 0xf1, 0xff, 0x0d, 0xd9, 0xb6, 0x0f,
   0xb1, 0x6d, 0x0d, 0xf9, 0xff, 0x0f, 0x91, 0x24, 0x0d, 0xb9, 0x6d, 0x0f,
   0xf1, 0xff, 0x0d, 0xa9, 0xaa, 0x0e, 0xfd, 0xff, 0x0f, 0xff, 0xff, 0x0f};
 
#define sm_dir_width 20
#define sm_dir_height 16
static char sm_dir_bits[] = {
   0xff, 0xff, 0x0f, 0x01, 0x00, 0x08, 0x01, 0x00, 0x0c, 0x51, 0x55, 0x0d,
   0xa9, 0xaa, 0x0e, 0x01, 0x15, 0x0c, 0x29, 0x80, 0x0e, 0x51, 0x55, 0x0d,
   0xa9, 0xaa, 0x0e, 0x51, 0x55, 0x0d, 0xa9, 0xaa, 0x0e, 0x51, 0x55, 0x0d,
   0xa9, 0xaa, 0x0e, 0x51, 0x55, 0x0d, 0xfd, 0xff, 0x0f, 0xff, 0xff, 0x0f};
 
>MENU DZDOC
 
>Help_item HELP_Zebra
 
>guidance
 
.          ***  The Zebra-Browser ***
.
Selecting "Zebra" from the KUIP object browser will display
an icon for each Zebra store, for each open Fzfile and each
open Rzfile. A store is named with its number plus the name
given to MZSTOR with slashes (/) replaced by underscores (_).
A Fzfile is named with the logical unit number, a Rzfile with
the top directory given to it by RZFILE.
.
.
Double click in the store symbol will display icons for the
divisions in this store, clicking the divisions will display
the banks im this divisions, clicking a bank will show the
tree for this bank in the graphics window (see DZDISP).
Popup menus (use right mouse key) are provided which allow to
print information on stores, divisions and banks (DZSTOR,
DZSNAP, DZSURV). The layout of banks in divisions can be
shown graphically by the command 'Display_division'.
.
.
Double click in the FZfile symbol will read the next data
structure from the file into division 1 of store 0 and
display the bank tree for the top bank in the graphics
window. A popup menu allows to read the User header only,
to display information (see FZINFO) on the file or close the
file. Trying to read beyond End of Data will force the
file to be rewound to avoid exit via ZFATAL.
.
.
Double click in the RZfile symbol will display directories
and/or keys contained in the file. Double click on a key
symbol will read the data structure for this key into
division 1 of store 0 and display the bank tree for the
top bank in ths graphics window. Show status (RZSTAT) and
close are accessible by a popup menu.
.
.          ****  Embedded DZDOC  ****
.
The Zebra browser popup menus give access to parts of the
DZDOC package. DZDOC allows to describe Zebra banks (linkage
and data words) with a defined format.
It puts this description into a Rzfile for direct access
and makes it available to DZDISP which can then display the
data words of a bank together with their meaning.
.
.
Opening, updating and using of the Rzfile can be done in the
same session. The file is opened via the menu item
'Open_bank_doc_Rzfile', one has to choose if it should be
opened 'New' for 'Update' or 'Readonly'.
Information can put into the file using the item
'Put_doc_into_Rzfile', for more information see help for this
command.
.
An alternative way is to use the item 'Edit_documentation' in
the popup menu associated with each bank (in the browser or
graphics window). In this case the editor is called with the
documentation contained in an (open) Rzfile if it is there.
If not a template with the documentation generated from the
selected bank in memory is presented in the editor.
If the file gets modified the documentation will be put into
the Rzfile if it is open for update. Note that the file
naming convention uses the hollerith Id of a bank
concatinated with the Id of its up-bank ('none' for a top
bank) with the extension '.dzdoc'.
.
.
.             ****   Vector/histogram filling   ****
.
The data words of a bank may be copied into a KUIP vector which
is optionally directly plotted or drawn (VECTOR/PLOT or DRAW).
This is done from the popup menu in the graphics or Icon
display of the ZEBRA banks. The vectors may then further be
manipulated by all the commands of the vector package.
This only works within paw++ where the VECTOR facilities are
linked in.
.
.
.             ****   Ntuple filling   ****
.
A simple facility is provided to mark words in a data structure
read from an FZ-file for filling into a Ntuple. The entries may
be scalar variables (i.e. one value of a bank) or arrays.
Marking of data words may be done explicitly giving the absolute
offset in a bank (a range in case of an array) or symbolically
if a (correct) documentation for the bank is available. In the
second case all values of a variable (max 100) in a bank are
extracted and filled into the Ntuple.
.
The marking is done from the popup menu associated to banks in
the graphics display, the system finds and remembers the links
to the bank.
.
Filling is controlled from the popup menu for the FZ-file icon.
For further details consult the Help items for these commands.
 
>Menu Set_par
>Guidance
Set various parameters:
Enable/disable workstation activation.
Select the two keys and the cycle to be shown by the browser.
DZDOC may optionally use a different window (workstation IWKID).
for its output.
CDZOPT Option when calling DZSHOW
 
>Command GeomFont
>Guidance
Set scale factor for size of banktree picture and
text fonts/precision
>Parameters
PSCALE 'Scale factor' R D=1.
ITFONT 'Text font in bank display' I D=-6
ITPREC 'Text prec in bank display' I D=0
>Action DZDSKD
 
>Command BankColor
>Guidance
Choose if bank display should be colored.
Color indexes for bank center and box.
>Parameters
COLBNK 'Color bank display' C D='Yes' R='Yes,No'
+
IBCCOL 'Color for bank center' I D=7
IBDCOL 'Color for data box' I D=3
>Action DZDSKD
 
>Command WorkStation
>Guidance
Enable/disable workstation activation.
DZDOC may optionally use a different window (workstation IWKID).
>Parameters
CCACT  'Activate workstation' C D='No' R='No,Yes'
IWKID  'Use workstation Id' I D=1
>Action DZDSKD
 
>Command BrowseKey
>Guidance
Select the two keys and the cycle to be shown by the browser.
>Parameters
IKDSP1 'Key to display' I D=1
IKDSP2 'Key to display' I D=2
ICYDSP 'Cycle to display' I D=1000000
>Action DZDSKD
 
>Command Ref_links
>Guidance
Select if reference links should be followed
>Parameters
CREFLN 'Follow reference links' C D='Yes' R='No,Yes'
>Action DZDSKD
 
>Command DZshow_Opt
>Guidance
CDZOPT Option when calling DZSHOW
>Parameters
CDZOPT 'DZSHOW option' C D='.'
>Action DZDSKD
 
>Command New_Window
>Guidance
Select if output of DZDISP etc. should go in a new window or
be displayed on the terminal (KXTERM).
>Parameters
CNEWIN 'Open new windows' C D='Yes' R='No,Yes'
>Action DZDSKD
 
>MENU ../File
 
>Command Open_doc
>Guidance
Open  (RZ) file with bank documentation in
Readonly (default), New or Update mode
Option L: List contents (RZLDIR) (file must be open)
>Parameters
CHDOCB 'File name' C D=' '
+
CHOPT  'Options' C D='R' R='R,N,U,L'
>Action DZDORF
 
>COMMAND Edit_doc
>GUIDANCE
Edit documentation for selected bank. The source may be
taken from:
         - an (open) RZ documentation file
         - an Ascii file in current dir or one added by DIR_ADD
         - a template of a bank in memory (Store and div must be given)
In the case of 'Ascii' the file name may explicitly be given in CHFILE,
if CHFILE = ' ' it is defaulted to 'CHBANK_CHUPID.dzdoc'.
>Parameters
CHBANK 'Bank Hol_Id  ' C D=' '
CHUPID 'UpBank Hol_Id' C D=' '
CHOPT  'Option       ' C D='Rz' R='Rz,Ascii,Memory'
CHFILE 'File name    ' C D=' '
STORE  'Store        ' I D=0
Div    'Division     ' I D=2
>ACTION DZDEDD
 
>Command Dir_add
>Guidance
Add a directory into search path when looking for
bank documention files. (max 10 dirs allowed)
>Parameters
CHDIR 'Directory name' C D=' '
>Action DZDDAD
 
>MENU ../Bankdisplay
 
>COMMAND Tree
>GUIDANCE
Show bank tree for selected bank
>Parameters
CHBANK 'Bank Hol_Id' C D=' '
+
NID    'Num Id       ' I D=0
STORE  'Store        ' I D=0
Div    'Division     ' I D=2
CHUPID 'UpBank Hol_Id' C D=' '
>ACTION DZDSHA
 
>COMMAND Contents
>GUIDANCE
Show contents of bank documented.
>Parameters
CHBANK 'Bank   Hol_Id' C D=' '
+
NID    'Num Id       ' I D=0
STORE  'Store        ' I D=0
Div    'Division     ' I D=2
CHUPID 'UpBank Hol_Id' C D=' '
>ACTION DZDSHA
 
>COMMAND DZshow_cont
>GUIDANCE
Show contents of bank by DZSHOW (no documentation)
>Parameters
CHBANK 'Bank Hol_Id' C D=' '
+
NID    'Num Id       ' I D=0
STORE  'Store        ' I D=0
Div    'Division     ' I D=2
CHUPID 'UpBank Hol_Id' C D=' '
>ACTION DZDSHA
 
>COMMAND Systemwords
>GUIDANCE
Show system words and links for selected bank
>Parameters
CHBANK 'Bank Hol_Id' C D=' '
+
NID    'Num Id       ' I D=0
STORE  'Store        ' I D=0
Div    'Division     ' I D=2
CHUPID 'UpBank Hol_Id' C D=' '
>ACTION DZDSHA
 
>COMMAND Documentation
>GUIDANCE
Show documentation for selected bank
>Parameters
CHBANK 'Bank Hol_Id' C D=' '
+
NID    'Num Id       ' I D=0
STORE  'Store        ' I D=0
Div    'Division     ' I D=2
CHUPID 'UpBank Hol_Id' C D=' '
>ACTION DZDSHA
 
>COMMAND Survey
>GUIDANCE
Call DZSURV for selected bank
>Parameters
CHBANK 'Bank Hol_Id' C D=' '
+
NID    'Num Id       ' I D=0
STORE  'Store        ' I D=0
Div    'Division     ' I D=2
CHUPID 'UpBank Hol_Id' C D=' '
>ACTION DZDSHA
 
>MENU ../BROWSE
 
>GUIDANCE
The commands in this menu are accessible only via the browser
since they need as argument an identification given internally
by the callback routine.
 
>COMMAND MARK_NT
>GUIDANCE
Mark data word(s) for Ntuple. An explicit word number
or range of words may be given, in this case CHNAME is
only used as a tag.
If blank its defaulted to: Bankid+IWORD (8 Characters)
 
If IWORDF is 0, it is assumed that a bank doc is available,
and CHNAME is the name of a variable in the doc.
If used with the option "array" all values with this name
in the bank will be filled (max. 100)
 
CHBANK:   internal bank numbering, dont touch
 
CHOPT:    Switch between single variables and arrays.
          'single'  mark a single (scalar) variable
          'array'  put each occurence of a variable in bank
          into an array and call HFNT once per event
IWORDF:
        if > 0 data
        -1     length of bank (not yet implemented)
        -2     number of links (nyi)
        -5     numerical Id of bank (nyi)
        -10    the link offset (Jbias of bank in Upbank) (nyi)
IWORDL:
        > IWORDF  last word to be marked (only with "array")
 
Option: 'L' loop on all other links (Jbias) to this bank
            in Upbank or on all next banks if it is part
            of a linear structure (nyi)
>Parameters
IDBANK 'Int Bank Id (DONT TOUCH)' I D=0
CHOPT  'Single variable or array' C D='Array' R='Single,Array'
IWORDF 'First word number'        I D=0
IWORDL 'Last word number'         I D=0
CHNAME 'Mnemonic (variable name)' C D=' '
COPT   'Option'                   C D='.'
>ACTION DZDSPI
 
>COMMAND SH_FOUT
>GUIDANCE
Write bank tree to FZFILE
>Parameters
IDBANK 'Int Bank Id (DONT TOUCH)' I D=0
LUNFOU 'Unit number' I D=0
CHBANK  'Holl Id'          C  D=' '
CHOPT   'Option for FZOUT' C D=' '
CHEAD   'Write user header' C D='Yes'
>ACTION DZDSPI
 
>COMMAND SH_DROP
>GUIDANCE
Drop bank (tree)
>Parameters
IDBANK 'Int Bank Id (DONT TOUCH)' I D=0
CHOPT   'Option for MZDROP' C D='.'
>ACTION DZDSPI
 
>COMMAND SH_MODIFY
>GUIDANCE
Modify data words of bank. Data words IFWORD -
ILWORD will be filled with IVALUE, RVALUE or CVALUE
depending on the type given in the IO-Char.
If more than 1 word are modified the type of the
first taken.
The integer value is interpreted as hexadecimal if
preceeded by $ or 16x.
If ILWORD=0 its set to IFWORD.
HINT+WARNING:
If the dataword offset is <=0 you can modify system words
on your own risk.
>Parameters
IDBANK 'Int Bank Id (DONT TOUCH)' I D=0
IFWORD   'First word' I D=' '
ILWORD   'Last word' I D=' '
IVALUE   'Integer value' C D=' '
RVALUE   'Real value'    R D=0.
CVALUE   'Char*4 value'  C D='? ? '
>ACTION DZDSPI
 
>COMMAND PT_VECT
>GUIDANCE
Put bank contents into a vector,
If CHVECT is blank name is taken from hollerith Id.
If IFWORD and ILWORD are 0 all data words are taken.
With option PLOT the values are filled in a histogram
which is then plotted (see VECTOR/PLOT), DRAW interprets
the data words as histogram contents (VECTOR/DRAW).
>Parameters
IDBANK 'Int Bank Id (DONT TOUCH)' I D=0
IDHIST 'Histogram Id' I D=12345
CHVECT 'Vector Name' C D=' '
IFWORD 'First word' I D=0
ILWORD 'Last word'  I D=0
CHOPT  'Option'     C D='DRAW' R='DRAW,PLOT,   '
>ACTION DZDSPI
 
>COMMAND SHOW_RZS
>GUIDANCE
Show status of a RZ file
>Parameters
CHPATH 'Rz directory' C D=' '
>ACTION DZDSRS
 
>COMMAND GET_KEY
>GUIDANCE
Read d/s of a RZ key into memory
>Parameters
CHPATH 'Rz directory' C D=' '
CHKNUM 'Key sequential num' C D=' '
>ACTION DZDGKY
 
>COMMAND SH_KEY
>GUIDANCE
Show key words
>Parameters
CHPATH 'Rz directory' C D=' '
CHKNUM 'Key sequential num' C D=' '
>ACTION DZDGKY
 
>COMMAND SH_KEY_DEF
>GUIDANCE
Show tags of keys
>Parameters
CHPATH 'Rz directory' C D=' '
>ACTION DZDGKY
 
 
>COMMAND SH_RZLDIR
>GUIDANCE
List directory (RZLDIR)
>Parameters
CHPATH 'Rz directory' C D=' '
>ACTION DZDGKY
 
 
>COMMAND FILT_KEY
>GUIDANCE
Set a filter on keywords (max first 9),
Keys created with the A option may be shown
optionally.
 
>Parameters
IKEYNR 'Keyword number' I D=1
IVALUE 'Integer value'  I D=0
CVALUE 'Char*4 (hollerith) value'  C D=' '
CHOPT  'Option'         C D='Int' R='Int,Char,Reset'
CHYENO 'List keys with Append opt' C D='NO' R='NO,YES'
>ACTION DZDSFK
 
>COMMAND FILT_BANKS
>GUIDANCE
Set a mask on hollerith and/or numerical Id of a bank.
The wildcard character is * or blank (i.e. CIDMSK = ' '
selects all).
Example: C**S , display all banks with Id starting with C
and ending with S.
If NIDMSK /= 0 display only banks with this numerical Id.
>Parameters
CIDMSK 'Mask Hol Id' C D='    '
NIDMSK 'Numerical Id' I D=0
>ACTION DZDSFB
 
>COMMAND CLOSE_RZ
>GUIDANCE
Close RZ file
>Parameters
CHPATH 'Rz directory' C D=' '
>ACTION DZDCRZ
 
>COMMAND PUT_EVENT
>GUIDANCE
Write bank tree to FZFILE read by GET_EVENT, if CHBANK /= ' '
the hollerith Id of the top bank is checked first
>Parameters
LUNFOU 'Unit number' I D=0
+
CHBANK  'Bank Hol_Id' C D=' '
CHOPT   'Option for FZOUT' C D=' '
CHEAD   'Write user header' C D='Yes'
>ACTION DZDPEV
 
>COMMAND CLOSE_FZ
>GUIDANCE
Close FZ file
>Parameters
CHLUN 'Unit number' C D=' '
>ACTION DZDCFZ
 
>COMMAND REWIND_FZ
>GUIDANCE
Rewind FZ file
>Parameters
CHLUN 'Unit number' C D=' '
>ACTION DZDRWF
 
>COMMAND FZ_LOG_LEVEL
>GUIDANCE
Set logging level for FZ file
>Parameters
CHLUN 'Unit number' C D=' '
LOGL  'Log level' I R=-3:4
>ACTION DZDFLL
 
>COMMAND GET_EVENT
>GUIDANCE
Read next d/s into memory and display it, if CHDS = 'D'
>Parameters
CHLUN 'Unit number' C D=' '
+
CHDS  'Display flag' C D=' '
>ACTION DZDFZI
 
>COMMAND GET_HEADER
>GUIDANCE
Read next User header into memory
>Parameters
CHLUN 'Unit number' C D=' '
+
CHDS  'Display flag' C D=' '
>ACTION DZDFZI
 
>COMMAND GET_PDS
>GUIDANCE
Read the pending d/s into memory
>Parameters
CHLUN 'Unit number' C D=' '
+
CHDS  'Display flag' C D=' '
>ACTION DZDFZI
 
>COMMAND SH_FZINFO
>GUIDANCE
Show Fz information for file open on Unit CHLUN
>Parameters
CHLUN 'Unit number' C D=' '
>ACTION DZDFZS
 
>COMMAND LIST_NTUPLE
>GUIDANCE
List currently marked variables
>ACTION DZDPNT
 
>COMMAND FILL_NTUPLE
>GUIDANCE
Read events from FZfile and fill Ntuple
RZPATH may contain the directory in an (open)
Rzfile to receive the Ntuple.
Option: 'R' Rewind file before filling
>Parameters
LUN  'Unit number'        I
NTUPID 'Id for Ntuple'      I D=99
CNAME  'Ntuple title'       C D='Zbrowse'
RZPATH 'Dir of Rzfile'      C D=' '
NEVENT 'Number of events'   I D=100000
CHOPT  'Option'             C D='.'
>ACTION DZDFNT
 
>COMMAND SHOW_STORE
>GUIDANCE
Call DZSTOR, CHSTOR is given in format:Store01
Default: Div 2 of current Store
>Parameters
CHSTO 'Store name' C D=' '
>ACTION DZDSHD
 
>COMMAND SHOW_DIV
>GUIDANCE
Call DZDDIV, CHDIV is given in format:Store01/Div03.
Default: Div 2 of current Store
>Parameters
CHDIV 'Div name' C D=' '
>ACTION DZDSHD
 
>COMMAND COL_GARB
>GUIDANCE
Call MZGARB for selected division
>Parameters
CHDIV 'Div name' C D=' '
>ACTION DZDSHD
 
>COMMAND SNAP_DIV
>GUIDANCE
Call DZSNAP, CHDIV is given in format:Store01/Div03.
Default: Div 2 of current Store
>Parameters
CHDIV 'Div name' C D=' '
>ACTION DZDSHD
 
>COMMAND VERIFY_DIV
>GUIDANCE
Call DZVERI, CHDIV is given in format:Store01/Div03.
Default: Div 2 of current Store
>Parameters
CHDIV 'Div name' C D=' '
CHOPT 'Option'   C D='CLSU'
>ACTION DZDSHD
 
************************************************************************
 
